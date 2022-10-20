unit munit;

{$mode objfpc}{$H+} 

interface

uses
  cmem, raylib, teApplication, teModelEngine;

type
TGame = class(TTinyApplication)
  private
  protected
    Engine: TTinyModelEngine;
    Camera:TCamera;
    Model:TTinyModel;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
    procedure Resized; override;
  end;

implementation

constructor TGame.Create;
begin
end;

procedure TGame.Init;
begin
  inherited Init;
  InitWindow(800, 600, 'raylib [Game Project]');
  SetWindowState(FLAG_VSYNC_HINT or FLAG_MSAA_4X_HINT);
  SetTargetFPS(60);

   Engine:= TTinyModelEngine.Create;
   Engine.DrawDebugGrid:=True;

   Camera.position:=Vector3Create( 1.5, 1.5, 1.5 ); // Camera position ;
   Camera.target := Vector3Create( 0.0, 0.4, 0.0 );      // Camera looking at point
   Camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
   Camera.fovy := 45.0;                                  // Camera field-of-view Y
   Camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type
   SetCameraMode(Camera, CAMERA_ORBITAL);

   // FModel := LoadModel('data/models/Spacesuit.m3d'); // Load the animated model mesh and basic data
   Model:=TTinyModel.Create(Engine);
   Model.LoadModel('data/models/test.m3d');
end;

procedure TGame.Update;
begin
  inherited Update;

  If IsKeyPressed(KEY_UP) then Model.PositionZ:=Model.PositionZ+0.01;

  Model.RotationAxis:=Vector3Create(90,0,0);

  UpdateCamera(@camera);

  Engine.Update;
end;

procedure TGame.Render;
begin
  inherited Render;
  Engine.Render(Camera);
end;

procedure TGame.Resized;
begin
  inherited Resized;
end;

procedure TGame.Shutdown;
begin
  inherited Shutdown;
end;

end.

