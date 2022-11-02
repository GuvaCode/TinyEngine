unit teFreeCamera;


{$mode ObjFPC}{$H+}

interface

uses
  raylib, rlgl, raymath, math;

type
  PTinyFreeCamera = ^TTinyFreeCamera;
  TTinyFreeCamera = record
    Position:TVector3;
    Forward_:TVector3;
    Up:TVector3;
    Cam: TCamera3d;
  end;

  // called to initialize a camera to default values
  procedure FreeCam_Init(camera:PTinyFreeCamera; position: TVector3; up: TVector3);

  // called to fill out a raylib camera
  procedure FreeCam_ToCamera(camera:PTinyFreeCamera; raylibCamera:PCamera3D);

  // called to set the target the camera should look at
  procedure FreeCam_LookAt(camera:PTinyFreeCamera; target: TVector3; up:TVector3);

  // called to set where the camera is
  procedure FreeCam_SetPosition(camera:PTinyFreeCamera; position:TVector3);

  // Called to move the camera relative to it's current orientation
  procedure FreeCam_MoveForward(camera:PTinyFreeCamera; distance: single);
  procedure FreeCam_MoveUp(camera:PTinyFreeCamera; distance: single);
  procedure FreeCam_MoveRight(camera:PTinyFreeCamera; distance: single);

  // Called to rotate the camera relative to it's current orientation
  procedure FreeCam_RotateYaw(camera:PTinyFreeCamera; angle: single);
  procedure FreeCam_RotatePitch(camera:PTinyFreeCamera; angle: single);
  procedure FreeCam_RotateRoll(camera:PTinyFreeCamera; angle: single);

  procedure FreeCameraUpdate(camera: PTinyFreeCamera);
  // called to rotate the camera around a world axis (Y or Z)
  procedure FreeCam_RotateHeading(camera:PTinyFreeCamera; angle: single; useY: boolean);
    // start drawing using the camera
  procedure FreeCameraBeginMode3D(camera: PTinyFreeCamera);
  // end drawing with the camera
  procedure FreeCameraEndMode3D;


implementation

procedure FreeCam_Init(camera: PTinyFreeCamera; position: TVector3; up: TVector3);
begin
  camera^.Position:=position;
  camera^.Up:=up;
      if abs(camera^.Up.z) > 0.1 then
          camera^.Forward_ := Vector3Create( 0, 1, 0 )
      else
          camera^.Forward_ := Vector3Create( 0, 0, 1 );
  camera^.Cam.fovy:=45;
end;

procedure FreeCam_ToCamera(camera: PTinyFreeCamera; raylibCamera: PCamera3D);
begin
  raylibCamera^.position := camera^.Position;
  raylibCamera^.target := Vector3Add(camera^.Position, camera^.Forward_);
  raylibCamera^.up := camera^.Up;
end;

procedure FreeCam_LookAt(camera: PTinyFreeCamera; target: TVector3; up: TVector3);
begin
  camera^.Forward_ := Vector3Normalize(Vector3Subtract(target, camera^.Position));
  camera^.Up := Vector3Normalize(up);
end;

procedure FreeCam_SetPosition(camera: PTinyFreeCamera; position: TVector3);
begin
  camera^.Position := camera^.Position;
end;

procedure FreeCam_MoveForward(camera: PTinyFreeCamera; distance: single);
begin
  camera^.Position := Vector3Add(camera^.Position, Vector3Scale(camera^.Forward_, distance));
end;

procedure FreeCam_MoveUp(camera: PTinyFreeCamera; distance: single);
begin
  camera^.Position := Vector3Add(camera^.Position, Vector3Scale(camera^.Up, distance));
end;

function GetRightVector(camera: PTinyFreeCamera): TVector3;
begin
result:= Vector3CrossProduct(camera^.Forward_, camera^.Up);
end;

procedure FreeCam_MoveRight(camera: PTinyFreeCamera; distance: single);
begin
  camera^.Position := Vector3Add(camera^.Position, Vector3Scale(GetRightVector(camera), distance));
end;

procedure FreeCam_RotateYaw(camera: PTinyFreeCamera; angle: single);
var matrix:TMatrix;
begin
   matrix:= MatrixRotate(camera^.Up, DEG2RAD * angle);
  camera^.Forward_ := Vector3Normalize(Vector3Transform(camera^.Forward_, matrix));
end;

procedure FreeCam_RotatePitch(camera: PTinyFreeCamera; angle: single);
var matrix:TMatrix;
begin
  angle := fmod(angle, 360);
  if angle < 0 then angle += 360;
  matrix := MatrixRotate(GetRightVector(camera), DEG2RAD * -angle);
  camera^.Up := Vector3Normalize(Vector3Transform(camera^.Up, matrix));
  camera^.Forward_ := Vector3Normalize(Vector3Transform(camera^.Forward_, matrix));
end;

procedure FreeCam_RotateRoll(camera: PTinyFreeCamera; angle: single);
var matrix:TMatrix;
begin
  matrix := MatrixRotate(camera^.Forward_, DEG2RAD * angle);
  camera^.Up := Vector3Normalize(Vector3Transform(camera^.Up, matrix));
end;

procedure FreeCameraUpdate(camera: PTinyFreeCamera);
begin
  FreeCam_ToCamera(camera,@camera^.cam);
end;

procedure FreeCam_RotateHeading(camera: PTinyFreeCamera; angle: single;
  useY: boolean);
var matrix:TMatrix;
begin
 if useY then
        matrix := MatrixRotateY(DEG2RAD * angle)
    else
        matrix := MatrixRotateZ(DEG2RAD * angle);

    camera^.Up := Vector3Normalize(Vector3Transform(camera^.Up, matrix));
    camera^.Forward_ := Vector3Normalize(Vector3Transform(camera^.Forward_, matrix));
end;

procedure FreeCameraBeginMode3D(camera: PTinyFreeCamera);
begin
  BeginMode3d(camera^.Cam);
end;

procedure FreeCameraEndMode3D;
begin
 EndMode3D;
end;



end.
