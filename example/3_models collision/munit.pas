unit munit;

{$mode objfpc}{$H+}

interface

uses
 raylib, raymath, teApplication, teModelEngine;

type
{ TBlock }

TBlock = class(TTinyModel)
  public
  procedure UpdateBB;
end;
{ TCharacter }

TCharacter = class(TTinyPlayerModel)
private
  FOldPosition: TVector3;
  public
  constructor Create(Engine: TTinyModelEngine); override;
  procedure Update; override;
  procedure DoCollision(CollisonModel: TTinyModel); override;
  property OldPosition: TVector3 read FOldPosition write FOldPosition;
end;

{ TGame }

TGame = class(TTinyApplication)
  private
  protected
    Engine: TTinyModelEngine;
    Camera:TCamera;
    Player:TCharacter;
    procedure LoadLevel;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
    procedure Resized; override;
  end;

implementation
uses Classes, SysUtils;

{ TBlock }
procedure TBlock.UpdateBB;
var cbb: TBoundingBox;
begin
  cbb:= GetModelBoundingBox(Model);
  cbb.min:=Vector3Scale(cbb.min,Scale);
  cbb.max:=Vector3Scale(cbb.max,Scale);
  cbb.min:=Vector3Add(cbb.min,Position);
  cbb.max:=Vector3Add(cbb.max,Position);
  CollisionBBox:=cbb;
end;

{ TCharacter }

constructor TCharacter.Create(Engine: TTinyModelEngine);
begin
  inherited Create(Engine);
  LoadModel('data/models/Character.m3d');
end;

procedure TCharacter.Update;
var cbb: TBoundingBox;

begin
  inherited Update;

 if IsKeyDown(Key_A) or IsKeyDown(KEY_LEFT) or IsGamepadButtonDown(0,GAMEPAD_BUTTON_LEFT_FACE_LEFT) then
  begin
    Speed:=1.5;
    Direction:=180;
    RotationAxis:=Vector3Create(0,270,0);
    AnimationIndex:=12;
  end else
 if IsKeyDown(Key_D) or IsKeyDown(KEY_RIGHT) or IsGamepadButtonDown(0,GAMEPAD_BUTTON_LEFT_FACE_RIGHT) then
  begin
    Speed:=1.5;
    Direction:=0;
    RotationAxis:=Vector3Create(0,90,0);
    AnimationIndex:=12;
  end else
  if IsKeyDown(Key_W) or IsKeyDown(KEY_UP) or IsGamepadButtonDown(0,GAMEPAD_BUTTON_LEFT_FACE_UP) then
   begin
     Speed:=1.5;
     Direction:=270;
     RotationAxis:=Vector3Create(0,180,0);
     AnimationIndex:=12;
   end else
   if IsKeyDown(Key_S) or IsKeyDown(KEY_DOWN) or IsGamepadButtonDown(0,GAMEPAD_BUTTON_LEFT_FACE_DOWN) then
    begin
      Speed:=1.5;
      Direction:=90;
      RotationAxis:=Vector3Create(0,0,0);
      AnimationIndex:=12;
    end else
    begin
      Speed:=0;
      AnimationIndex:=4;
    end;

    cbb:= GetModelBoundingBox(Model);
    cbb.min:=Vector3Scale(cbb.min,Scale);
    cbb.max:=Vector3Scale(cbb.max,Scale);
    cbb.min:=Vector3Add(cbb.min,Position);
    cbb.max:=Vector3Add(cbb.max,Position);
    CollisionBBox:=cbb;

    Collision;

    OldPosition:=Position;
end;

procedure TCharacter.DoCollision(CollisonModel: TTinyModel);
begin
   if CollisonModel is TBlock then
    begin
    Position:=OldPosition;
    end;
end;

procedure TGame.LoadLevel;
var
  level:TStringList;
  ax,az: Integer;

begin
   level := TStringList.Create;
   level.LoadFromFile('data/map/level_1');

   for az := 0 to level.Count - 1 do
     begin
      for ax := 1 to length(level[az]) do
        begin
          case level[az][ax] of
          'x':  //Dirt
            with TBlock.Create(Engine) do
              begin
                LoadModel('data/models/Cube_Dirt_Center_Tall.m3d');
                Scale:=0.5;
                PositionX:= ax * Scale;
                PositionZ:= az * Scale;
              end;
            's': //single
            with TBlock.Create(Engine) do
              begin
                LoadModel('data/models/Cube_Grass_Single.m3d');
                Scale:=0.5;
                PositionX:= ax * Scale;
                PositionZ:= az * Scale;
                PositionY:= Scale;
                UpdateBB;
                Collisioned:=True;
              end;
              'b': //brick
            with TBlock.Create(Engine) do
              begin
                LoadModel('data/models/Cube_Bricks.m3d');
                Scale:=0.5;
                PositionX:= ax * Scale;
                PositionZ:= az * Scale;
                PositionY:= Scale;
                UpdateBB;
                Collisioned:=True;
              end;
            'm': //bomb
            with TBlock.Create(Engine) do
              begin
                LoadModel('data/models/Bomb.m3d');
                Scale:=0.5;
                PositionX:= ax * Scale;
                PositionZ:= az * Scale;
                PositionY:= Scale;
                CollisionMode:=cmSphere;
                CollisionSphere:=Position;
                CollisionRadius:=0.3;
                Collisioned:=True;
              end;
            'p': //player
            with Player do
              begin
                AnimationIndex:=4;
                AnimationSpeed:=40;
                Scale:=0.5;
                PositionX:= ax * Scale;
                PositionZ:= az * Scale;
                RotationAxis:=Vector3Create(0,0,0);
                Camera.target := Player.Position; // set camera to player position
                Collisioned:=True;
              end;
          end;
        end;
     end;
    level.Free;
end;

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
   Engine.DrawBebugModel:=False;

   Camera.position:=Vector3Create( 0, 10, 10 );          // Camera position ;
   Camera.target := Vector3Create( 0.0, 0.0, 0.0 );      // Camera looking at point
   Camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
   Camera.fovy := 15.0;                                  // Camera field-of-view Y
   Camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type
   SetCameraMode(Camera, CAMERA_FREE);

   Player:=TCharacter.Create(Engine);

   LoadLevel;
end;

procedure TGame.Update;
begin
  inherited Update;

  UpdateCamera(@camera);

  Camera.target:=Player.Position; // target camera to player position

  Engine.Update;  // update engine

  if IsKeyReleased(Key_M) then Engine.DrawBebugModel:= not Engine.DrawBebugModel;
end;

procedure TGame.Render;
begin
  inherited Render;
  Engine.Render(Camera); // render engine

  DrawText('press w,a,s,d or cursor key or gamepad for moved', 10 , 10 , 10, GRAY);
  DrawText('press m for show/hide debug draw collision', 10 , 20 , 10, GRAY);
  DrawText('(c) model by @quaternius', 660 , 580, 10, GRAY);
end;

procedure TGame.Resized;
begin
  inherited Resized;
end;

procedure TGame.Shutdown;
begin
  Engine.ClearDeadModel;
  Engine.Free;
  inherited Shutdown;
end;

end.

