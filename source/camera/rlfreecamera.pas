unit rlFreeCamera;


{$mode ObjFPC}{$H+}

interface

uses
  raylib, rlgl, raymath, math;

type
  PrlFreeCamera = ^TrlFreeCamera;
  TrlFreeCamera = record
    Position:TVector3;
    Forward_:TVector3;
    Up:TVector3;
    Cam: TCamera3d;
  end;

  // called to initialize a camera to default values
  procedure rlFreeCam_Init(camera:PrlFreeCamera; position: TVector3; up: TVector3);

  // called to fill out a raylib camera
  procedure rlFreeCam_ToCamera(camera:PrlFreeCamera; raylibCamera:PCamera3D);

  // called to set the target the camera should look at
  procedure rlFreeCam_LookAt(camera:PrlFreeCamera; target: TVector3; up:TVector3);

  // called to set where the camera is
  procedure rlFreeCam_SetPosition(camera:PrlFreeCamera; position:TVector3);

  // Called to move the camera relative to it's current orientation
  procedure rlFreeCam_MoveForward(camera:PrlFreeCamera; distance: single);
  procedure rlFreeCam_MoveUp(camera:PrlFreeCamera; distance: single);
  procedure rlFreeCam_MoveRight(camera:PrlFreeCamera; distance: single);

  // Called to rotate the camera relative to it's current orientation
  procedure rlFreeCam_RotateYaw(camera:PrlFreeCamera; angle: single);
  procedure rlFreeCam_RotatePitch(camera:PrlFreeCamera; angle: single);
  procedure rlFreeCam_RotateRoll(camera:PrlFreeCamera; angle: single);

  procedure rlFreeCameraUpdate(camera: PrlFreeCamera);
  // called to rotate the camera around a world axis (Y or Z)
  procedure rlFreeCam_RotateHeading(camera:PrlFreeCamera; angle: single; useY: boolean);
    // start drawing using the camera
  procedure rlFreeCameraBeginMode3D(camera: PrlFreeCamera);
  // end drawing with the camera
  procedure rlFreeCameraEndMode3D;


implementation

procedure rlFreeCam_Init(camera: PrlFreeCamera; position: TVector3; up: TVector3);
begin
  camera^.Position:=position;
  camera^.Up:=up;
      if abs(camera^.Up.z) > 0.1 then
          camera^.Forward_ := Vector3Create( 0, 1, 0 )
      else
          camera^.Forward_ := Vector3Create( 0, 0, 1 );
  camera^.Cam.fovy:=45;
end;

procedure rlFreeCam_ToCamera(camera: PrlFreeCamera; raylibCamera: PCamera3D);
begin
  raylibCamera^.position := camera^.Position;
  raylibCamera^.target := Vector3Add(camera^.Position, camera^.Forward_);
  raylibCamera^.up := camera^.Up;
end;

procedure rlFreeCam_LookAt(camera: PrlFreeCamera; target: TVector3; up: TVector3);
begin
  camera^.Forward_ := Vector3Normalize(Vector3Subtract(target, camera^.Position));
  camera^.Up := Vector3Normalize(up);
end;

procedure rlFreeCam_SetPosition(camera: PrlFreeCamera; position: TVector3);
begin
  camera^.Position := camera^.Position;
end;

procedure rlFreeCam_MoveForward(camera: PrlFreeCamera; distance: single);
begin
  camera^.Position := Vector3Add(camera^.Position, Vector3Scale(camera^.Forward_, distance));
end;

procedure rlFreeCam_MoveUp(camera: PrlFreeCamera; distance: single);
begin
  camera^.Position := Vector3Add(camera^.Position, Vector3Scale(camera^.Up, distance));
end;

function GetRightVector(camera: PrlFreeCamera): TVector3;
begin
result:= Vector3CrossProduct(camera^.Forward_, camera^.Up);
end;

procedure rlFreeCam_MoveRight(camera: PrlFreeCamera; distance: single);
begin
  camera^.Position := Vector3Add(camera^.Position, Vector3Scale(GetRightVector(camera), distance));
end;

procedure rlFreeCam_RotateYaw(camera: PrlFreeCamera; angle: single);
var matrix:TMatrix;
begin
   matrix:= MatrixRotate(camera^.Up, DEG2RAD * angle);
  camera^.Forward_ := Vector3Normalize(Vector3Transform(camera^.Forward_, matrix));
end;

procedure rlFreeCam_RotatePitch(camera: PrlFreeCamera; angle: single);
var matrix:TMatrix;
begin
  angle := fmod(angle, 360);
  if angle < 0 then angle += 360;
  matrix := MatrixRotate(GetRightVector(camera), DEG2RAD * -angle);
  camera^.Up := Vector3Normalize(Vector3Transform(camera^.Up, matrix));
  camera^.Forward_ := Vector3Normalize(Vector3Transform(camera^.Forward_, matrix));
end;

procedure rlFreeCam_RotateRoll(camera: PrlFreeCamera; angle: single);
var matrix:TMatrix;
begin
  matrix := MatrixRotate(camera^.Forward_, DEG2RAD * angle);
  camera^.Up := Vector3Normalize(Vector3Transform(camera^.Up, matrix));
end;

procedure rlFreeCameraUpdate(camera: PrlFreeCamera);
begin
  rlFreeCam_ToCamera(camera,@camera^.cam);
end;

procedure rlFreeCam_RotateHeading(camera: PrlFreeCamera; angle: single;
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

procedure rlFreeCameraBeginMode3D(camera: PrlFreeCamera);
begin
  BeginMode3d(camera^.Cam);
end;

procedure rlFreeCameraEndMode3D;
begin
 EndMode3D;
end;



end.
