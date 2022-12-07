unit munit;

{$mode objfpc}{$H+}

interface

uses
 raylib, rlgl, raymath, teApplication, teModelEngine, teLights;

const
GLSL_VERSION = 330;

type
{ TGame }
TGame = class(TTinyApplication)
  private
  protected
    Engine: TTinyModelEngine;
    Camera:TCamera;
    modelPlain: TModel;
    player:TTinyModel;
    shader: TShader;
    lights: array [0..MAX_LIGHTS] of TLight ;
    shaderVol, cameraPos: array [0..3] of single;
    ShaderIndex: integer;
  public
    ambientLoc: integer;
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
    procedure Resized; override;
  end;

implementation
uses Classes, SysUtils;

constructor TGame.Create;
begin
end;

procedure TGame.Init;
var i: integer;
begin
  inherited Init;
  InitWindow(800, 600, 'raylib [Game Project]');
  SetWindowState(FLAG_VSYNC_HINT or FLAG_MSAA_4X_HINT);
  //SetTargetFPS(60);
   self.ClearBackgroundColor:=black;
  Engine:= TTinyModelEngine.Create;
  Engine.DrawDebugGrid:=true;


  player:=TTinyModel.Create(Engine);
  player.LoadModel('data/models/Trapdoor_open_mod.m3d');
  player.PositionY:=0.5;
  //player.Scale:=0.2;

  // Define the camera to look into our 3d world
  camera.position := Vector3Create(2.0,4.0,6.0);    // Camera position
  camera.target := Vector3Create(0.0,0.5,0.0);      // Camera looking at point
  camera.up := Vector3Create(0.0,1.0, 0.0);         // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                              // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;          // Camera mode type

  // Load plane model from a generated mesh
 // modelPlain := LoadModelFromMesh(GenMeshPlane(10.0, 10.0, 3, 3));

  shader := LoadShader(TextFormat('data/shaders/glsl%i/lighting.vs', GLSL_VERSION),
  TextFormat('data/shaders/glsl%i/lighting.fs', GLSL_VERSION));

  // Get some required shader loactions
   shader.locs[SHADER_LOC_VECTOR_VIEW] := GetShaderLocation(shader, 'viewPos');
  // NOTE: "matModel" location name is automatically assigned on shader loading,
  // no need to get the location again if using that uniform name
 // shader.locs[SHADER_LOC_MATRIX_MODEL] := GetShaderLocation(shader, 'matModel');

  // Ambient light level (some basic lighting)
  ambientLoc := GetShaderLocation(shader, 'ambient');

  shaderVol[0]:=0.0;
  shaderVol[1]:=0.1;
  shaderVol[2]:=0.1;
  shaderVol[3]:=1.1;
  SetShaderValue(shader, ambientLoc, @shaderVol, SHADER_UNIFORM_VEC4);
 //matProjection
 ambientLoc := GetShaderLocation(shader, 'matProjection');



  for i:= 0 to player.Model.materialCount-1 do
  player.Model.materials[i].shader:= shader;

  // Using 4 point lights: gold, red, green and blue
  lights[0] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1.5 , -2 ), Vector3Zero(), WHITE, shader);
  lights[1] := CreateLight(LIGHT_POINT, Vector3Create(  2, 1.5 ,  2 ), Vector3Zero(), WHITE, shader);
  lights[2] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1.5 ,  2 ), Vector3Zero(), WHITE, shader);
  lights[3] := CreateLight(LIGHT_POINT, Vector3Create(  2, 1.5 , -2 ), Vector3Zero(), WHITE, shader);

  SetCameraMode(camera, CAMERA_ORBITAL);  // Set an orbital camera mode


end;

procedure TGame.Update;
var j:integer;
begin
  inherited Update;

  UpdateCamera(@camera);

       // Check key inputs to enable/disable lights
  if IsKeyPressed(KEY_Y) then  lights[0].enabled := not lights[0].enabled;
  if IsKeyPressed(KEY_R) then  lights[1].enabled := not lights[1].enabled;
  if IsKeyPressed(KEY_G) then  lights[2].enabled := not lights[2].enabled;
  if IsKeyPressed(KEY_B) then  lights[3].enabled := not lights[3].enabled;

  if IsKeyReleased(KEY_DOWN) then ShaderIndex:=11;// if ShaderIndex < 25 then Inc(ShaderIndex);

  // Update light values (actually, only enable/disable them)
  for  j := 0 to MAX_LIGHTS do UpdateLightValues(shader, lights[j]);

  // Update the shader with the camera view vector (points towards { 0.0f, 0.0f, 0.0f })
  cameraPos[0] := camera.position.x;
  cameraPos[1] := camera.position.y;
  cameraPos[2] := camera.position.z;

  SetShaderValue(shader, shader.locs[SHADER_LOC_MAP_SPECULAR], @cameraPos, SHADER_UNIFORM_VEC3);
  Engine.Update;  // update engine

end;

procedure TGame.Render;
var i: integer;
begin
  inherited Render;

  BeginMode3D(camera);
 // DrawModel(modelPlain, Vector3Zero(), 1.0, WHITE);

   // Draw spheres to show where the lights are
   for i:=0 to MAX_LIGHTS-1 do
   if (lights[i].enabled) then DrawSphereEx(lights[i].position, 0.2, 8, 8, lights[i].color)
   else
   DrawSphereWires(lights[i].position, 0.2, 8, 8, ColorAlpha(lights[i].color, 0.3));

  Engine.Render(Camera); // render engine
  EndMode3D();
  DrawFPS(10, 10);
  DrawText('Use keys [Y][R][G][B] to toggle lights', 10, 40, 20, DARKGRAY);
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

