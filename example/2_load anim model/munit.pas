unit munit;

{$mode objfpc}{$H+} 

interface

uses
  cmem, raylib, teApplication, teModelEngine, sysutils;

type
TGame = class(TTinyApplication)
  private
  protected
    Engine: TTinyModelEngine;
    Camera:TCamera;
    Model:TTinyAnimatedModel;
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

   Camera.position:=Vector3Create( 1.5, 1.5, 1.5 );       // Camera position ;
   Camera.target := Vector3Create( 0.0, 0.4, 0.0 );      // Camera looking at point
   Camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
   Camera.fovy := 45.0;                                  // Camera field-of-view Y
   Camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type
   SetCameraMode(Camera, CAMERA_ORBITAL);


   Model:=TTinyAnimatedModel.Create(Engine);
   Model.LoadModel('data/models/Bunny.m3d');
   Model.AnimationLoop:=true;
   Model.AnimationIndex:=0;
   Model.AnimationSpeed:=30 ;
end;

procedure TGame.Update;
begin
  inherited Update;

  UpdateCamera(@camera);

  Engine.Update;

  If IsKeyPressed(KEY_UP) then Model.AnimationIndex:=Model.AnimationIndex + 1;
  If IsKeyPressed(KEY_DOWN) then Model.AnimationIndex:=Model.AnimationIndex - 1;

  If IsKeyPressed(KEY_LEFT) then Model.AnimationSpeed:=Model.AnimationSpeed - 1;
  If IsKeyPressed(KEY_RIGHT) then Model.AnimationSpeed:=Model.AnimationSpeed + 1;

end;

procedure TGame.Render;
begin
  inherited Render;
  Engine.Render(Camera);

  DrawText(PChar('Press UP or DOWN to change model animation index'),10,10,10,BLACK);
  DrawText(PChar('Press LEFT or RIGHT to change model speed animation'),10,20,10,BLACK);

  DrawText(PChar('Animation index = ' + IntTostr(Model.AnimationIndex))  ,10,40,10, RED);
  DrawText(PChar('Animation speed = ' + FloatTostr(Model.AnimationSpeed))  ,10,50,10, RED);

  DrawText('(c) model by @quaternius', 660 , 580, 10, GRAY);
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

