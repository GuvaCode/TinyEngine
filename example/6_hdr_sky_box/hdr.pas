unit hdr;

{$mode objfpc}{$H+} 

interface

uses
  cmem, raylib, raymath, rlgl, teApplication;

const GLSL_VERSION = 330;

type

{ TGame }

TGame = class(TTinyApplication)
  private
  protected
    Camera:TCamera;
    shdrCubemap: TShader;
    panoramaScr: TTexture2D;
    cube: TMesh;
    skybox: TModel;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
    procedure Resized; override;
    function GenTextureCubemap(shader: Tshader; panorama_: TTexture2D; size, format: Integer): TTextureCubemap;
  end;

implementation

constructor TGame.Create;
begin
end;

procedure TGame.Init;
var map: Integer;
    useHDR: boolean;
    cbDat: integer;
begin
   inherited Init;
   InitWindow(800, 600, 'raylib [Game Project]');
   SetWindowState(FLAG_VSYNC_HINT or FLAG_MSAA_4X_HINT);
   SetTargetFPS(60);
   ClearBackgroundColor:=GRAY;

   Camera.position:=Vector3Create( 1.0, 1.0, 1.0 ); // Camera position ;
   Camera.target := Vector3Create( 4.0, 1.0, 4.0 );      // Camera looking at point
   Camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
   Camera.fovy := 45.0;                                  // Camera field-of-view Y
   Camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type

   // Load skybox model
   cube := GenMeshCube(1.0, 1.0, 1.0);
   skybox := LoadModelFromMesh(cube);

   // Load skybox shader and set required locations
   // NOTE: Some locations are automatically set at shader loading
   skybox.materials[0].shader := LoadShader(TextFormat('data/shaders/glsl%i/skybox.vs', GLSL_VERSION),
                                            TextFormat('data/shaders/glsl%i/skybox.fs', GLSL_VERSION));

   map:=MATERIAL_MAP_CUBEMAP;
   useHDR:= true;
   SetShaderValue(skybox.materials[0].shader, GetShaderLocation(skybox.materials[0].shader, 'environmentMap'), @map, SHADER_UNIFORM_INT);
   SetShaderValue(skybox.materials[0].shader, GetShaderLocation(skybox.materials[0].shader, 'doGamma'), @useHDR, SHADER_UNIFORM_INT);
   SetShaderValue(skybox.materials[0].shader, GetShaderLocation(skybox.materials[0].shader, 'vflipped'), @useHDR, SHADER_UNIFORM_INT);



   // Load cubemap shader and setup required shader locations
   shdrCubemap := LoadShader(TextFormat('data/shaders/glsl%i/cubemap.vs', GLSL_VERSION),
                             TextFormat('data/shaders/glsl%i/cubemap.fs', GLSL_VERSION));

   SetShaderValue(shdrCubemap, GetShaderLocation(shdrCubemap, 'equirectangularMap'), @cbDat, SHADER_UNIFORM_INT);

   panoramaScr := LoadTexture('data/image/dresden_square_2k.hdr');

   skybox.materials[0].maps[MATERIAL_MAP_CUBEMAP].texture :=
   GenTextureCubemap(shdrCubemap, panoramaScr, 1024 , PIXELFORMAT_UNCOMPRESSED_R8G8B8A8);


   SetCameraMode(camera, CAMERA_FIRST_PERSON);  // Set a first person camera mode
end;

procedure TGame.Update;
begin
  inherited Update;

  UpdateCamera(@camera);


end;

procedure TGame.Render;
begin
  inherited Render;
    BeginMode3D(camera);
      // We are inside the cube, we need to disable backface culling!
      rlDisableBackfaceCulling();
      rlDisableDepthMask();
      DrawModel(skybox, Vector3Zero, 1.0, WHITE);
      rlEnableBackfaceCulling();
      rlEnableDepthMask();
   EndMode3D();
end;

procedure TGame.Resized;
begin
  inherited Resized;
end;

function TGame.GenTextureCubemap(shader: Tshader; panorama_: TTexture2D; size,
  format: Integer): TTextureCubemap;
var i: integer;
    rbo, fbo: LongWord;
    cubemap: TTextureCubemap;
    matFboProjection: TMatrix;
    fboViews: array [0..5] of TMatrix;

begin

  rlDisableBackfaceCulling; // Disable backface culling to render inside the cube
  // STEP 1: Setup framebuffer
  // ---------------------------------------------------------------------------
  rbo := rlLoadTextureDepth(size, size, true);

  cubemap.id := rlLoadTextureCubemap(pointer(nil), size, format);

  fbo := rlLoadFramebuffer(size, size);
  rlFramebufferAttach(fbo, rbo, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_RENDERBUFFER, 0);
  rlFramebufferAttach(fbo, cubemap.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_CUBEMAP_POSITIVE_X, 0);

  // Check if framebuffer is complete with attachments (valid)
  if rlFramebufferComplete(fbo) then TraceLog(LOG_INFO, 'FBO: [ID %i] Framebuffer object created successfully', fbo);
  // ---------------------------------------------------------------------------

  // STEP 2: Draw to framebuffer
  // ---------------------------------------------------------------------------
  // NOTE: Shader is used to convert HDR equirectangular environment map to cubemap equivalent (6 faces)
  rlEnableShader(shader.id);

  // Define projection matrix and send it to shader
  matFboProjection := MatrixPerspective(90.0*DEG2RAD, 1.0, RL_CULL_DISTANCE_NEAR, RL_CULL_DISTANCE_FAR);
  rlSetUniformMatrix(shader.locs[SHADER_LOC_MATRIX_PROJECTION], matFboProjection);

  // Define view matrix for every side of the cubemap
  fboViews[0] := MatrixLookAt(Vector3Create(0.0, 0.0, 0.0), Vector3Create( 1.0, 0.0, 0.0), Vector3Create(0.0, -1.0, 0.0));
  fboViews[1] := MatrixLookAt(Vector3Create(0.0, 0.0, 0.0), Vector3Create(-1.0, 0.0, 0.0), Vector3Create(0.0, -1.0, 0.0));
  fboViews[2] := MatrixLookAt(Vector3Create(0.0, 0.0, 0.0), Vector3Create(0.0, 1.0, 0.0), Vector3Create(0.0, 0.0, 1.0));
  fboViews[3] := MatrixLookAt(Vector3Create(0.0, 0.0, 0.0), Vector3Create(0.0, -1.0, 0.0), Vector3Create(0.0, 0.0, -1.0));
  fboViews[4] := MatrixLookAt(Vector3Create(0.0, 0.0, 0.0), Vector3Create(0.0, 0.0, 1.0), Vector3Create(0.0, -1.0, 0.0));
  fboViews[5] := MatrixLookAt(Vector3Create(0.0, 0.0, 0.0), Vector3Create(0.0, 0.0, -1.0), Vector3Create(0.0, -1.0, 0.0));

  rlViewport(0, 0, size, size); // Set viewport to current fbo dimensions

  // Activate and enable texture for drawing to cubemap faces
  rlActiveTextureSlot(0);
  rlEnableTexture(panorama_.id);


  for i := 0 to 5 do
    begin
      // Set the view matrix for the current cube face
      rlSetUniformMatrix(shader.locs[SHADER_LOC_MATRIX_VIEW], fboViews[i]);

      // Select the current cubemap face attachment for the fbo
      // WARNING: This function by default enables->attach->disables fbo!!!
      rlFramebufferAttach(fbo, cubemap.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_CUBEMAP_POSITIVE_X + i, 0);
      rlEnableFramebuffer(fbo);

      // Load and draw a cube, it uses the current enabled texture
      rlClearScreenBuffers;
      rlLoadDrawCube;
    end;

  // STEP 3: Unload framebuffer and reset state
  // ---------------------------------------------------------------------------
  rlDisableShader;             // Unbind shader
  rlDisableTexture;            // Unbind texture
  rlDisableFramebuffer;        // Unbind framebuffer
  rlUnloadFramebuffer(fbo);    // Unload framebuffer (and automatically attached depth texture/renderbuffer)

  // Reset viewport dimensions to default
  rlViewport(0, 0, rlGetFramebufferWidth, rlGetFramebufferHeight);
  rlEnableBackfaceCulling;

  cubemap.width := size;
  cubemap.height := size;
  cubemap.mipmaps := 1;
  cubemap.format := format;

  result := cubemap;
end;

procedure TGame.Shutdown;
begin
  inherited Shutdown;
end;

end.

