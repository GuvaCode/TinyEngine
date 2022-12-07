unit munit;

{$mode objfpc}{$H+} 

interface

uses
  cmem, raylib, rayGui, raymath,
  teApplication, teModelEngine, teLights,
  SysUtils, Classes, FileUtil;

const
GLSL_VERSION = 330;

type

{ TGame }
TGame = class(TTinyApplication)
  private
  protected
    Engine: TTinyModelEngine;
    Pack: TTinyModelPack;
    Camera:TCamera;
    Model:TTinyAnimatedModel;
    Shader: TShader;
    Lights: array [0..MAX_LIGHTS] of TLight ;
    ColorValue: array [0..MAX_LIGHTS] of  TColorB;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
    procedure Resized; override;
    procedure LoadFromName(i: Integer);
    procedure ApplyShader(Model_: TModel);
    procedure SetVolShader(vol: Single);
    procedure SetLight;
  end;

var Current: Integer;
    listViewActive: longint = 0;
    listViewScrollIndex: longint = 0;
    /// = (r: 255; g: 255; b: 255; a: 255);


implementation

constructor TGame.Create;
begin
end;

procedure TGame.Init;
var ModelsList: TStringList;
    i: integer;
begin
  inherited Init;
  InitWindow(900, 600, 'raylib [Game Project]');
  SetWindowState(FLAG_VSYNC_HINT or FLAG_MSAA_4X_HINT);
  SetTargetFPS(60);
  ClearBackgroundColor:=LIGHTGRAY;

  Engine:= TTinyModelEngine.Create;
  Engine.DrawDebugGrid:=True;

  Pack:= TTinyModelPack.Create;

  ModelsList:= FindAllFiles('data/models/character', '*.m3d', false);

  try
   for i:=0 to ModelsList.Count -1 do
   Pack.LoadFromFile(ModelsList.Strings[i]);
  finally
    ModelsList.Free;
  end;

   Camera.position:=Vector3Create( 1.5, 1.5, 1.5 );       // Camera position ;
   Camera.target := Vector3Create( 0.0, 0.4, 0.0 );      // Camera looking at point
   Camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
   Camera.fovy := 45.0;                                  // Camera field-of-view Y
   Camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type
   SetCameraMode(Camera, CAMERA_ORBITAL);

   // Load and settings light shader
   Shader := LoadShader(TextFormat('data/shaders/glsl%i/lighting.vs', GLSL_VERSION),
   TextFormat('data/shaders/glsl%i/lighting.fs', GLSL_VERSION));

   Shader.locs[SHADER_LOC_VECTOR_VIEW] := GetShaderLocation(shader, 'viewPos');
   SetVolShader(0.1);

   Model:=TTinyAnimatedModel.Create(Engine);
   Model.LoadFromModelPack(Current,Pack);
   Model.AnimationLoop:=true;
   Model.AnimationIndex:=0;
   Model.AnimationSpeed:=30 ;

   //apply shader to model
   ApplyShader(Model.Model);

   for i:=0 to 3 do ColorValue[i]:=ColorCreate(255,255,255,255);

  lights[0] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1.5 , -2 ), Vector3Zero, ColorValue[0], shader);
  lights[1] := CreateLight(LIGHT_POINT, Vector3Create(  2, 1.5 ,  2 ), Vector3Zero, ColorValue[1], shader);
  lights[2] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1.5 ,  2 ), Vector3Zero, ColorValue[2], shader);
  lights[3] := CreateLight(LIGHT_POINT, Vector3Create(  2, 1.5 , -2 ), Vector3Zero, ColorValue[3], shader);

end;

procedure TGame.Update;
begin
  inherited Update;

  UpdateCamera(@camera);

  Engine.Update;

  if IsKeyPressed(KEY_UP) then Model.AnimationIndex:=Model.AnimationIndex + 1;
  if IsKeyPressed(KEY_DOWN) then Model.AnimationIndex:=Model.AnimationIndex - 1;

  if IsKeyPressed(KEY_LEFT) then Model.AnimationSpeed:=Model.AnimationSpeed - 1;
  if IsKeyPressed(KEY_RIGHT) then Model.AnimationSpeed:=Model.AnimationSpeed + 1;

  if IsKeyReleased(KEY_A) then
    begin
      if current > 0 then Dec(Current);
      Model.LoadFromModelPack(Current,Pack);
    end;

  if IsKeyReleased(KEY_S) then
    begin
      if current < Pack.Count-1 then Inc(Current);
      Model.LoadFromModelPack(Current,Pack);
      ApplyShader(Model.Model);
    end;
end;

procedure TGame.Render;
var oldAct: LongInt;
    i: Integer;
    oldColor: array [0..3] of  TColorB;
begin
  inherited Render;
  //Engine render
  Engine.Render(Camera);

  BeginMode3D(camera);
  // Draw ligh sphere
  for i:=0 to 3 do
  DrawSphereWires(lights[i].position, 0.2, 8, 8, ColorAlpha(lights[i].color, 0.3));
  EndMode3D();

  //Gui render
  oldAct:=listViewActive;
  listViewActive := GuiListView(RectangleCreate( 10, 130, 140, 240 ),
  'Tribal;Monkroose;Fish;Dino;Demon;Cactoro;Bunny', @listViewScrollIndex, listViewActive);

  for i:=0 to 3 do oldColor[i]:=ColorValue[i];
  ColorValue[0] := GuiColorPicker(RectangleCreate( 770, 10, 96, 92 ),'Color picker', ColorValue[0]);
  ColorValue[1] := GuiColorPicker(RectangleCreate( 770, 110, 96, 92 ),'Color picker', ColorValue[1]);
  ColorValue[2] := GuiColorPicker(RectangleCreate( 770, 210, 96, 92 ),'Color picker', ColorValue[2]);
  ColorValue[3] := GuiColorPicker(RectangleCreate( 770, 310, 96, 92 ),'Color picker', ColorValue[3]);

  DrawText(PChar('FPS: ' + IntToStr(GetFPS)),10,580,10, RED);
  DrawText(PChar('Select to use load from name procedure'),10,115,10,BLACK);
  DrawText(PChar('Press UP or DOWN to change model animation index'),10,10,10,BLACK);
  DrawText(PChar('Press LEFT or RIGHT to change model speed animation'),10,20,10,BLACK);
  DrawText(PChar('Press A or S for change model'),10,30,10,BLACK);
  DrawText(PChar('Animation index = ' + IntToStr(Model.AnimationIndex))  ,10,60,10, RED);
  DrawText(PChar('Animation speed = ' + FloatToStr(Model.AnimationSpeed))  ,10,70,10, RED);
  DrawText(PChar('Model name : ' + PChar(Model.ModelName))  ,10,80,10, RED);
  DrawText(PChar('Model index = ' + IntToStr(Current))  ,10,90,10, RED);
  DrawText('(c) model by @quaternius', 660 , 580, 10, GRAY);

  if oldAct <>  listViewActive then LoadFromName(listViewActive);

  for i:= 0 to 3 do
  if (OldColor[i].r <> ColorValue[i].r) or
     (OldColor[i].g <> ColorValue[i].g) or
     (OldColor[i].b <> ColorValue[i].b) then SetLight;
end;

procedure TGame.Resized;
begin
  inherited Resized;
end;

procedure TGame.LoadFromName(i: Integer);
begin
  case i of
  0: Model.LoadFromModelPack('Tribal',Pack);
  1: Model.LoadFromModelPack('Monkroose',Pack);
  2: Model.LoadFromModelPack('Fish',Pack);
  3: Model.LoadFromModelPack('Dino',Pack);
  4: Model.LoadFromModelPack('Demon',Pack);
  5: Model.LoadFromModelPack('Cactoro',Pack);
  6: Model.LoadFromModelPack('Bunny',Pack);
  end;
  Current:=i;
  ApplyShader(self.Model.Model);
end;

procedure TGame.ApplyShader(Model_: TModel);
var i: integer;
begin
  for i:= 0 to Model_.materialCount-1 do
  Model_.materials[i].shader:= shader;
end;

procedure TGame.SetVolShader(vol: Single);
var ShaderVol: array [0..3] of single;
    i,ambientLoc: integer;
begin
  ambientLoc := GetShaderLocation(shader, 'ambient');
  for i:=0 to 3 do shaderVol[i]:=vol;
  SetShaderValue(shader, ambientLoc, @shaderVol, SHADER_UNIFORM_VEC4);
  ambientLoc := GetShaderLocation(shader, 'matProjection');
end;

procedure TGame.SetLight;
var i: integer;
begin
for i:=0 to 3 do
  begin
    lights[i].color:=ColorValue[i];
    UpdateLightValues(Shader,lights[i]);
  end;
end;

procedure TGame.Shutdown;
begin
  Pack.Free;
  inherited Shutdown;
end;

end.

