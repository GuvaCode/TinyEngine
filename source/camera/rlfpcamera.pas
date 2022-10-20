{
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃   raylibExtras * Utilities and Shared Components for Raylib                  ┃
┃                                                                              ┃
┃    rlFPCamera * First person camera                                          ┃
┃                                                                              ┃
┃   Original source code in C                                                  ┃
┃   https://github.com/raylib-extras/extras-c/blob/main/cameras                ┃
┃   Copyright (c) 2021 Jeffery Myers                                           ┃
┃                                                                              ┃
┃   Pacal translation (c) 2021 Gunko Vadim                                     ┃
┃   https://github.com/GuvaCode                                                ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
}
unit rlFPCamera;


{$mode ObjFPC}{$H+}

interface

uses
  raylib, rlgl, raymath, math;

type
  PrlFPCameraControls = ^TrlFPCameraControls;
  TrlFPCameraControls = longint;
    const
      MOVE_FRONT   = 0;
      MOVE_BACK    = 1;
      MOVE_RIGHT   = 2;
      MOVE_LEFT    = 3;
      MOVE_UP      = 4;
      MOVE_DOWN    = 5;
      TURN_LEFT    = 6;
      TURN_RIGHT   = 7;
      TURN_UP      = 8;
      TURN_DOWN    = 9;
      SPRINT       = 10;
      LAST_CONTROL = 11;

type
  PrlFPCamera = ^TrlFPCamera;
  TrlFPCamera = record
    ControlsKeys: array[0..LAST_CONTROL] of longint; // keys used to control the camera
    MoveSpeed: TVector3; // the speed in units/second to move. X = sidestep, Y = jump/fall, Z = forward
    TurnSpeed: TVector2; // the speed for turning when using keys to look degrees/second
    UseMouse: boolean; // use the mouse for looking?
    MouseSensitivity: single; // how many pixels equate out to an angle move, larger numbers mean slower, more accurate mouse
    MinimumViewY : single; // how far down can the camera look
    MaximumViewY : single; // how far up can the camera look
    ViewBobbleFreq: single; // how fast the view should bobble as you move. defaults to 0 for no bobble
    ViewBobbleMagnatude: single; // how high up/down will the bobble be
    ViewBobbleWaverMagnitude : single; // how far left and right should the view bobble
    CameraPosition: TVector3; // the position of the base of the camera (on the floor)
                              // note that this will not be the view position because it is offset by the eye height.
                              // this value is also not changed by the view bobble
    PlayerEyesPosition: single; // how far from the base of the camera is the player's view
    FOV: TVector2;  // the field of view in X and Y
    TargetDistance: single; // state for view movement
    ViewAngles: TVector2; // state for view angles
    CurrentBobble: single; // state for bobble
    Focused: boolean; // state for window focus
    AllowFlight: boolean;
    ViewCamera: TCamera3D; // raylib camera for use with raylib modes.
    Forwards : TVector3;
    Right: TVector3;
    //clipping planes
    // note must use BeginModeFP3D and EndModeFP3D instead of BeginMode3D/EndMode3D for clipping planes to work
    NearPlane: double;
    FarPlane: double;
  end;

  // called to initialize a camera to default values
  procedure rlFPCameraInit(camera: PrlFPCamera; fovY: single; position: TVector3);

  // called to update field of view in X when window resizes
  procedure rlFPCameraResizeView(camera: PrlFPCamera);

  // turn the use of mouselook on/off, also updates the cursor visibility
  procedure rlFPCameraUseMouse(camera: PrlFPCamera; useMouse: boolean);

  // Get the camera's position in world (or game) space
  function rlFPCameraGetPosition(camera: PrlFPCamera): TVector3;

  // Set the camera's position in world (or game) space
  procedure rlFPCameraSetPosition(camera: PrlFPCamera; pos: TVector3);

  // returns the ray from the camera through the center of the view
  function rlFPCameraGetViewRay(camera: PrlFPCamera): TRay;

  // update the camera for the current frame
  procedure rlFPCameraUpdate(camera: PrlFPCamera);

  // start drawing using the camera, with near/far plane support
  procedure rlFPCameraBeginMode3D(camera: PrlFPCamera);

  // end drawing with the camera
  procedure rlFPCameraEndMode3D;


implementation

procedure rlFPCameraInit(camera: PrlFPCamera; fovY: single; position: TVector3);
begin
  if camera = nil then exit;
  camera^.ControlsKeys[0]  := KEY_W;
  camera^.ControlsKeys[1]  := KEY_S;
  camera^.ControlsKeys[2]  := KEY_D;
  camera^.ControlsKeys[3]  := KEY_A;
  camera^.ControlsKeys[4]  := KEY_E;
  camera^.ControlsKeys[5]  := KEY_Q;
  camera^.ControlsKeys[6]  := KEY_LEFT;
  camera^.ControlsKeys[7]  := KEY_RIGHT;
  camera^.ControlsKeys[8]  := KEY_UP;
  camera^.ControlsKeys[9]  := KEY_DOWN;
  camera^.ControlsKeys[10] := KEY_LEFT_SHIFT;

  camera^.MoveSpeed := Vector3Create(1,1,1);
  camera^.TurnSpeed := Vector2Create(90,90);

  camera^.UseMouse := true;
  camera^.MouseSensitivity := 600;

  camera^.MinimumViewY := -89.0;
  camera^.MaximumViewY := 89.0;

  camera^.ViewBobbleFreq := 0.0;
  camera^.ViewBobbleMagnatude := 0.02;
  camera^.ViewBobbleWaverMagnitude := 0.002;
  camera^.CurrentBobble := 0;

  camera^.Focused := IsWindowFocused;

  camera^.TargetDistance := 1;
  camera^.PlayerEyesPosition := 0.5;
  camera^.ViewAngles := Vector2Create(0,0);

  camera^.CameraPosition := position;
  camera^.FOV.y := fovY;

  camera^.ViewCamera.position := position;
  camera^.ViewCamera.position.y += camera^.PlayerEyesPosition;
  camera^.ViewCamera.target := Vector3Add(camera^.ViewCamera.position, Vector3Create(0,0,camera^.TargetDistance));
  camera^.ViewCamera.up := Vector3Create(0.0,1.0,0.0);
  camera^.ViewCamera.fovy := fovY;
  camera^.ViewCamera.projection := CAMERA_PERSPECTIVE;

  camera^.AllowFlight := false;

  camera^.NearPlane := 0.01;
  camera^.FarPlane := 1000.0;

  rlFPCameraResizeView(camera);
  rlFPCameraUseMouse(camera, camera^.UseMouse);
end;

procedure rlFPCameraResizeView(camera: PrlFPCamera);
var width,height: single;
begin
   if camera = nil then exit;
   width := GetScreenWidth();
   height := GetScreenHeight();
   camera^.FOV.y := camera^.ViewCamera.fovy;
   if height <> 0 then camera^.FOV.x := camera^.FOV.y * (width / height);
end;

procedure rlFPCameraUseMouse(camera: PrlFPCamera; useMouse: boolean);
begin
  if camera = nil then exit;
  camera^.UseMouse := useMouse;
  if (useMouse and IsWindowFocused) then DisableCursor()
    else
  if (not useMouse and IsWindowFocused) then EnableCursor;
end;

function rlFPCameraGetPosition(camera: PrlFPCamera): TVector3;
begin
  result:=camera^.CameraPosition;
end;

procedure rlFPCameraSetPosition(camera: PrlFPCamera; pos: TVector3);
var forward_:TVector3;
begin
   camera^.CameraPosition := pos;
   forward_ := Vector3Subtract(camera^.ViewCamera.target, camera^.ViewCamera.position);
   camera^.ViewCamera.position := camera^.CameraPosition;
   camera^.ViewCamera.target := Vector3Add(camera^.CameraPosition, forward_);
end;

function rlFPCameraGetViewRay(camera: PrlFPCamera): TRay;
var rRay:TRay;
begin
  rRay.position:=camera^.CameraPosition;
  rRay.direction:=camera^.Forwards;
  result:=rRay;
end;

function GetSpeedForAxis(camera: PrlFPCamera; axis: TrlFPCameraControls; speed: single): single;
var key:longint;
    factor:single;
begin
  result:=0.0;
  if camera = nil then Exit;

    key := camera^.ControlsKeys[axis];

    if key = -1 then result:= 0;

    factor := 1.0;
    if IsKeyDown(camera^.ControlsKeys[SPRINT]) then
    factor := 2;

    if IsKeyDown(camera^.ControlsKeys[axis]) then
        result:= speed * GetFrameTime() * factor;
end;

procedure rlFPCameraUpdate(camera: PrlFPCamera);
var mousePositionDelta: TVector2;
    direction: array[0..MOVE_DOWN] of single;
    turnRotation, tiltRotation, eyeOfset, swingDelta, viewBobbleDampen: single;
    target: TVector3;
begin
    if camera = nil then exit;

    if IsWindowFocused() <> (camera^.Focused and camera^.UseMouse) then
    begin
        camera^.Focused := IsWindowFocused;
        if camera^.Focused then
          DisableCursor
        else
          EnableCursor;
    end;

    // Mouse movement detection
    mousePositionDelta := GetMouseDelta();

    // Keys input detection
    direction[MOVE_FRONT] := GetSpeedForAxis(camera,MOVE_FRONT,camera^.MoveSpeed.z);
    direction[MOVE_BACK]  := GetSpeedForAxis(camera,MOVE_BACK,camera^.MoveSpeed.z);
    direction[MOVE_RIGHT] := GetSpeedForAxis(camera,MOVE_RIGHT,camera^.MoveSpeed.x);
    direction[MOVE_LEFT]  := GetSpeedForAxis(camera,MOVE_LEFT,camera^.MoveSpeed.x);
    direction[MOVE_UP]    := GetSpeedForAxis(camera,MOVE_UP,camera^.MoveSpeed.y);
    direction[MOVE_DOWN]  := GetSpeedForAxis(camera,MOVE_DOWN,camera^.MoveSpeed.y);

    // let someone modify the projected position
    // Camera orientation calculation
    turnRotation := GetSpeedForAxis(camera, TURN_RIGHT, camera^.TurnSpeed.x) - GetSpeedForAxis(camera, TURN_LEFT, camera^.TurnSpeed.x);
    tiltRotation := GetSpeedForAxis(camera, TURN_UP, camera^.TurnSpeed.y) - GetSpeedForAxis(camera, TURN_DOWN, camera^.TurnSpeed.y);

    if turnRotation <> 0 then
        camera^.ViewAngles.x -= turnRotation * DEG2RAD
    else if (camera^.UseMouse and camera^.Focused) then
        camera^.ViewAngles.x += (mousePositionDelta.x / -camera^.MouseSensitivity);

    if tiltRotation <> 0 then
        camera^.ViewAngles.y += tiltRotation * DEG2RAD
    else if (camera^.UseMouse and camera^.Focused) then
        camera^.ViewAngles.y += (mousePositionDelta.y / -camera^.MouseSensitivity);

    // Angle clamp
    if camera^.ViewAngles.y < camera^.MinimumViewY * DEG2RAD then
        camera^.ViewAngles.y := camera^.MinimumViewY * DEG2RAD
    else if camera^.ViewAngles.y > camera^.MaximumViewY * DEG2RAD then
        camera^.ViewAngles.y := camera^.MaximumViewY * DEG2RAD;

    // Recalculate camera target considering translation and rotation
    target:= Vector3Transform(Vector3Create(0,0,1),MatrixRotateXYZ(Vector3Create(camera^.ViewAngles.y,-camera^.ViewAngles.x,0)));

    if camera^.AllowFlight then
        camera^.Forwards := target
    else
        camera^.Forwards := Vector3Transform(Vector3Create(0,0,1), MatrixRotateXYZ(Vector3Create(0,-camera^.ViewAngles.x,0)));

    camera^.Right:= Vector3Create(camera^.Forwards.z * -1.0, 0, camera^.Forwards.x );

    camera^.CameraPosition := Vector3Add(camera^.CameraPosition, Vector3Scale(camera^.Forwards, direction[MOVE_FRONT] - direction[MOVE_BACK]));
    camera^.CameraPosition := Vector3Add(camera^.CameraPosition, Vector3Scale(camera^.Right, direction[MOVE_RIGHT] - direction[MOVE_LEFT]));

    camera^.CameraPosition.y += direction[MOVE_UP] - direction[MOVE_DOWN];

    camera^.ViewCamera.position := camera^.CameraPosition;

    eyeOfset := camera^.PlayerEyesPosition;

    if camera^.ViewBobbleFreq > 0 then
    begin
        swingDelta := (max(abs(direction[MOVE_FRONT] - direction[MOVE_BACK]),
                       abs(direction[MOVE_RIGHT] - direction[MOVE_LEFT])));

        camera^.CurrentBobble += swingDelta * camera^.ViewBobbleFreq;
        viewBobbleDampen := 8.0;
        eyeOfset -= sin(camera^.CurrentBobble / viewBobbleDampen) * camera^.ViewBobbleMagnatude;
        camera^.ViewCamera.up.x := sin(camera^.CurrentBobble / (viewBobbleDampen * 2)) * camera^.ViewBobbleWaverMagnitude;
        camera^.ViewCamera.up.z := -sin(camera^.CurrentBobble / (viewBobbleDampen * 2)) * camera^.ViewBobbleWaverMagnitude;
    end
    else
    begin
        camera^.CurrentBobble := 0;
        camera^.ViewCamera.up.x := 0;
        camera^.ViewCamera.up.z := 0;
    end;
    camera^.ViewCamera.position.y += eyeOfset;
    camera^.ViewCamera.target.x := camera^.ViewCamera.position.x + target.x;
    camera^.ViewCamera.target.y := camera^.ViewCamera.position.y + target.y;
    camera^.ViewCamera.target.z := camera^.ViewCamera.position.z + target.z;
end;

procedure SetupCamera(camera: PrlFPCamera; aspect: single);
var top,right: double;
    matView: TMatrix;
    i: integer;
begin
    rlDrawRenderBatchActive();			// Draw Buffers (Only OpenGL 3+ and ES2)
    rlMatrixMode(RL_PROJECTION);        // Switch to projection matrix
    rlPushMatrix();                     // Save previous matrix, which contains the settings for the 2d ortho projection
    rlLoadIdentity();                   // Reset current matrix (projection)

    if camera^.ViewCamera.projection = CAMERA_PERSPECTIVE then
    begin
        // Setup perspective projection
         top := RL_CULL_DISTANCE_NEAR * tan(camera^.ViewCamera.fovy * 0.5 * DEG2RAD);
         right := top * aspect;
         rlFrustum(-right, right, -top, top, camera^.NearPlane, camera^.FarPlane);
    end
    else if (camera^.ViewCamera.projection = CAMERA_ORTHOGRAPHIC) then
    begin
        // Setup orthographic projection
         top := camera^.ViewCamera.fovy / 2.0;
         right := top * aspect;
         rlOrtho(-right, right, -top, top, camera^.NearPlane, camera^.FarPlane);
    end;

    // NOTE: zNear and zFar values are important when computing depth buffer values
    rlMatrixMode(RL_MODELVIEW);         // Switch back to modelview matrix
    rlLoadIdentity();                   // Reset current matrix (modelview)

    // Setup Camera view
    matView := MatrixLookAt(camera^.ViewCamera.position, camera^.ViewCamera.target, camera^.ViewCamera.up);
    rlMultMatrixf(MatrixToFloatV(matView).v);      // Multiply modelview matrix by view matrix (camera)
    rlEnableDepthTest();                // Enable DEPTH_TEST for 3D
end;

procedure rlFPCameraBeginMode3D(camera: PrlFPCamera);
var aspect: single;
begin
  aspect:=GetScreenWidth / GetScreenHeight;
  SetupCamera(camera, aspect);
end;

procedure rlFPCameraEndMode3D;
begin
  EndMode3D();
end;

end.

