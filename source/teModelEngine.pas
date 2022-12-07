unit teModelEngine;

{$mode objfpc}{$H+}

interface

uses
  raylib, raymath, rlgl,
  Classes, SysUtils, Generics.Collections;

type
  TTinyEngineDrawMode = (dmNormal, dmEx, dmWires, dmWiresEx); // draw model mode
  TTinyModelCollisionMode = (cmBBox,cmSphere);   // collision mode
  TTinyModelJumpState = (jsNone, jsJumping, jsFalling); // state for jump

  TTinyModel = class;

  { TTinyModelEngine }
  TTinyModelEngine = class
    private
      FDrawBebugModel: Boolean;
      FDrawDebugGrid: boolean;
      FDrawDistance: single;
      FDrawSkeleton: boolean;
      FGridSlice: longint;
      FGridSpace: single;
      function GetModel(const Index: Integer): TTinyModel;
      function GetModelsCount: Integer;
      procedure SetDrawBebugModel(AValue: Boolean);
      procedure SetDrawDebugGrid(AValue: boolean);
      procedure SetDrawDistance(AValue: single);
      procedure SetDrawSkeleton(AValue: boolean);
    protected
     FModelDeadList: specialize TList<TTinyModel>; // model dead list
     FModelList: specialize TList<TTinyModel>; // list of model
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(const Model: TTinyModel);
      procedure Remove(const Model: TTinyModel);
      procedure Change(Model: TTinyModel; Dest: TTinyModelEngine);
      procedure Update;  // update engine
      procedure Render(Camera: TCamera); virtual; // render engine
      procedure ClearDeadModel;  // clear of death model
      procedure SetDebugGrid(slices:longint; spacing:single); // set grid size and slice
      property Items[const Index: Integer]: TTinyModel read GetModel; default;
   published
      property DrawSkeleton: Boolean read FDrawSkeleton write SetDrawSkeleton;
      property DrawDebugGrid: Boolean read FDrawDebugGrid write SetDrawDebugGrid;
      property DrawBebugModel: Boolean read FDrawBebugModel write SetDrawBebugModel;
      property DrawDistance: Single read FDrawDistance write SetDrawDistance;
      property ModelsCount: Integer  read GetModelsCount;
  end;

  { TTinyModelPack }
  TTinyModelPack = class
    public
      Count: Integer;
      ModelName: array of string;
      Model: array of TModel;
      ModelAnimation: array of PModelAnimation;
      ModelAnimCount: array of Integer;
      function LoadFromFile(FileName: String): Boolean;
      constructor Create;
      destructor Destroy; override;
   end;

  { TTinyModel }
  TTinyModel = class
    private
      FCollisionBBox: TBoundingBox;
      FCollisioned: Boolean;
      FCollisionMode: TTinyModelCollisionMode;
      FCollisionRadius: Single;
      FCollisionSphere: TVector3;
      FDebugColor: TColorB;
      FDebugDraw: Boolean;
      FDrawMode: TTinyEngineDrawMode;
      FPosition: TVector3;
      FRotationAngle: single;
      FRotationAxis: TVector3;
      FScale: Single;
      FModelName: String;
      function GetPositionX: Single;
      function GetPositionY: Single;
      function GetPositionZ: Single;
      procedure SetCollisionBBox(AValue: TBoundingBox);
      procedure SetCollisionMode(AValue: TTinyModelCollisionMode);
      procedure SetCollisionRadius(AValue: Single);
      procedure SetCollisionSphere(AValue: TVector3);
      procedure SetDebugDraw(AValue: Boolean);
      procedure SetDrawMode(AValue: TTinyEngineDrawMode);
      procedure SetPosition(AValue: TVector3);
      procedure SetPositionX(AValue: Single);
      procedure SetPositionY(AValue: Single);
      procedure SetPositionZ(AValue: Single);
      procedure SetRotationAngle(AValue: single);
      procedure SetRotationAxis(AValue: TVector3);
      procedure SetScale(AValue: Single);
      procedure Collision(const OtherModel: TTinyModel); overload; virtual;
    protected
      FModelDead: Boolean;
      FEngine: TTinyModelEngine;
      FModel: TModel;
      FTexture: TTexture;
      procedure DoCollision(CollisonModel: TTinyModel); virtual;
    public
      constructor Create(ModelEngine: TTinyModelEngine); virtual;
      destructor Destroy; override;
      procedure Update; overload; virtual;
      procedure Render; virtual;
      procedure Dead;
      procedure LoadModel(FileName: String); virtual;
      procedure LoadModelTexture(TextureFileName: String; MaterialMap: TMaterialMapIndex);
      procedure LoadFromModelPack(Index: Integer; ModelPack: TTinyModelPack); virtual;
      procedure LoadFromModelPack(Name: String; ModelPack: TTinyModelPack); virtual;
      procedure Collision; overload; virtual;
      procedure Assign(const Value: TModel); virtual;
      property DrawMode: TTinyEngineDrawMode read FDrawMode write SetDrawMode;
      property Model: TModel read FModel write FModel;
      property RotationAxis: TVector3 read FRotationAxis write SetRotationAxis;
      property Position: TVector3 read FPosition write SetPosition;
      property PositionX: Single read GetPositionX write SetPositionX;
      property PositionY: Single read GetPositionY write SetPositionY;
      property PositionZ: Single read GetPositionZ write SetPositionZ;
      property RotationAngle: single read FRotationAngle write SetRotationAngle;
      property Scale: Single read FScale write SetScale;
      property ModelName: String read FModelName write FModelName;
      property DebugDraw: Boolean read FDebugDraw write SetDebugDraw;
      property DebugColor: TColorB read FDebugColor write FDebugColor;
      property Collisioned: Boolean read FCollisioned write FCollisioned;
      property CollisionMode: TTinyModelCollisionMode read FCollisionMode write SetCollisionMode;
      property CollisionBBox: TBoundingBox read FCollisionBBox write SetCollisionBBox;
      property CollisionSphere: TVector3 read FCollisionSphere write SetCollisionSphere;
      property CollisionRadius: Single read FCollisionRadius write SetCollisionRadius;
      property Engine: TTinyModelEngine read FEngine write FEngine;
  end;

  { TTinyAnimatedModel }
  TTinyAnimatedModel = class(TTinyModel)
  private
    FAnimationCount: Integer;
    FAnimationIndex: Integer;
    FAnimationLoop: boolean;
    FAnimationSpeed: Single;
    FAnimEnded: Boolean;
    FAnims: PModelAnimation;
    FAnimFrameCounter: Single;
    FAnimCont: Integer;
    procedure SetAnimationIndex(AValue: Integer);
    procedure SetAnimationLoop(AValue: boolean);
    procedure SetAnimationSpeed(AValue: Single);
  protected
    procedure UpdateModelAnimation;
  public
    constructor Create(ModelEngine: TTinyModelEngine); override;
    procedure AssignAnim(const Value: PModelAnimation);
    procedure Update; override;
    procedure LoadModel(FileName: String); override;
    procedure LoadFromModelPack(Index: integer; ModelPack: TTinyModelPack); override;
    procedure LoadFromModelPack(Name: String; ModelPack: TTinyModelPack); override;
    property Anims: PModelAnimation read FAnims write FAnims;
    property AnimationIndex: Integer read FAnimationIndex write SetAnimationIndex;
    property AnimationSpeed: Single read FAnimationSpeed write SetAnimationSpeed;
    property AnimationLoop: Boolean read FAnimationLoop write SetAnimationLoop;
    property AnimEnded: Boolean read FAnimEnded;
  end;

  { TTinyPlayerModel }
  TTinyPlayerModel = class(TTinyAnimatedModel)
  private
    FAcc: Single;
    FDcc: Single;
    FDirection: Single;
    FMaxSpeed: Single;
    FMinSpeed: Single;
    FRotation: Single;
    FSpeed: Single;
    FVelocity: TVector3;
    procedure SetDirection(AValue: Single);
    procedure SetRotation(AValue: Single);
    procedure SetSpeed(AValue: Single);
  public
    constructor Create(ModelEngine: TTinyModelEngine); override;
    procedure Update; override;
    procedure Accelerate; virtual;
    procedure Deccelerate; virtual;
    property Speed: Single read FSpeed write SetSpeed;
    property MinSpeed: Single read FMinSpeed write FMinSpeed;
    property MaxSpeed: Single read FMaxSpeed write FMaxSpeed;
    property Velocity: TVector3 read FVelocity write FVelocity;
    property Acceleration: Single read FAcc write FAcc;
    property Decceleration: Single read FDcc write FDcc;
    property Direction: Single read FDirection write SetDirection;
    property Rotation: Single read FRotation write SetRotation;
  end;

  var    cosTable : array[ 0..360 ] of Single;
         sinTable : array[ 0..360 ] of Single;

implementation
uses Math;

function m_Cos(Angle: Integer): Single;
begin
   if Angle > 360 Then
    DEC( Angle, ( Angle div 360 ) * 360 )
  else
    if Angle < 0 Then
      INC( Angle, ( abs( Angle ) div 360 + 1 ) * 360 );
  Result := cosTable[ Angle ];
end;

function m_Sin(Angle: Integer): Single;
begin
  if Angle > 360 Then
    DEC( Angle, ( Angle div 360 ) * 360 )
  else
    if Angle < 0 Then
      INC( Angle, ( abs( Angle ) div 360 + 1 ) * 360 );
  Result := sinTable[ Angle ];
end;

procedure InitCosSinTables;
var
  i         : Integer;
  rad_angle : Single;
begin
for i := 0 to 360 do
  begin
    rad_angle := i * ( pi / 180 );
    cosTable[ i ] := cos( rad_angle );
    sinTable[ i ] := sin( rad_angle );
  end;
end;

{ TTinyModelPack }
function TTinyModelPack.LoadFromFile(FileName: String): Boolean;
begin
  if not FileExists(PChar(FileName)) then
    begin
      Result := False;
      Exit;
    end;
  SetLength(Model, Count + 1);
  SetLength(ModelName, Count + 1);
  SetLength(ModelAnimation, Count + 1);
  SetLength(ModelAnimCount, Count + 1);
  Inc(Count);
  Model[Count - 1] := LoadModel(PChar(FileName));
  ModelName[Count - 1] := ChangeFileExt(ExtractFileName(FileName),'');
  ModelAnimation[Count - 1]:= nil;
  ModelAnimation[Count - 1]:= LoadModelAnimations(PChar(FileName), @ModelAnimCount[Count - 1]);
  Result := True;
end;

constructor TTinyModelPack.Create;
begin
//
end;

destructor TTinyModelPack.Destroy;
var  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    ModelName[i] := '';
    UnloadModel(Model[i]);
    UnloadModelAnimations(ModelAnimation[i],ModelAnimCount[i]);
    ModelAnimCount[i]:=0;
  end;
  SetLength(ModelName, 0);
  SetLength(Model, 0);
  SetLength(ModelAnimation, 0);
  SetLength(ModelAnimCount, 0);
  Count := 0;
  inherited Destroy;
end;

{ TTinyPlayerModel }
procedure TTinyPlayerModel.SetDirection(AValue: Single);
begin
  FDirection := AValue;
  FVelocity.x := m_Sin(Trunc(FDirection)) * Speed;
  FVelocity.z := m_Cos(Trunc(FDirection)) * Speed;
  FVelocity.y := sin(DEG2RAD * -FRotation) * Speed ;
end;

procedure TTinyPlayerModel.SetRotation(AValue: Single);
begin
  if FRotation=AValue then Exit;
  FRotation:=AValue;
end;

procedure TTinyPlayerModel.SetSpeed(AValue: Single);
begin
  if FSpeed > FMaxSpeed then FSpeed := FMaxSpeed
  else
  if FSpeed < FMinSpeed then  FSpeed := FMinSpeed;
  FSpeed := AValue;
  FVelocity.x := m_Sin(Trunc(FDirection)) * Speed;
  FVelocity.z := m_Cos(Trunc(FDirection)) * Speed;
  FVelocity.y := sin(DEG2RAD * -FRotation) * Speed ;
end;

constructor TTinyPlayerModel.Create(ModelEngine: TTinyModelEngine);
begin
  inherited Create(ModelEngine);
  FVelocity:=Vector3Create(0,0,0);
  Direction:=0;
  Acceleration:=0;
  Decceleration:=0;
  Speed:=0;
  MinSpeed:=0;
  MaxSpeed:=0;
end;

procedure TTinyPlayerModel.Update;
begin
  inherited Update;
  FPosition.x := FPosition.x + FVelocity.x * GetFrameTime;
  FPosition.z := FPosition.z + FVelocity.z * GetFrameTime;
  FPosition.y := FPosition.y + FVelocity.y * GetFrameTime;
end;

procedure TTinyPlayerModel.Accelerate;
begin
  if FSpeed <> FMaxSpeed then
 begin
   FSpeed := FSpeed + FAcc;
   if FSpeed > FMaxSpeed then
   FSpeed := FMaxSpeed;
   FVelocity.x := m_Cos(Trunc(FDirection))  * Speed;
   FVelocity.z := m_Sin(Trunc(FDirection))  * Speed;
   FVelocity.y := sin(DEG2RAD * -FRotation) * Speed;
 end;
end;

procedure TTinyPlayerModel.Deccelerate;
begin
  if FSpeed <> FMinSpeed then
 begin
   FSpeed := FSpeed - FDcc;
   if FSpeed < FMinSpeed then
   FSpeed := FMinSpeed;
   FVelocity.x := m_Cos(Trunc(FDirection))  * Speed;
   FVelocity.z := m_Sin(Trunc(FDirection))  * Speed;
   FVelocity.y := sin(DEG2RAD * -FRotation) * Speed;
 end;
end;

{ TTinyAnimatedModel }
procedure TTinyAnimatedModel.SetAnimationIndex(AValue: Integer);
begin
  if (AValue >= FAnimCont) or (Avalue < 0) or (FAnimationIndex = AValue)  then Exit;
  if FAnimationLoop = false then FAnimFrameCounter:=0;
  FAnimFrameCounter := 0;
  FAnimationIndex := AValue;
end;


procedure TTinyAnimatedModel.SetAnimationLoop(AValue: boolean);
begin
  if FAnimationLoop = AValue then Exit;
  FAnimationLoop := AValue;
end;

procedure TTinyAnimatedModel.SetAnimationSpeed(AValue: Single);
begin
  if (FAnimationSpeed = AValue) or (Avalue < 0) then Exit;
  FAnimationSpeed:=AValue;
end;

procedure TTinyAnimatedModel.UpdateModelAnimation;
begin
  try
  if IsModelAnimationValid(FModel, FAnims^) then
    begin
      FAnimFrameCounter:= FAnimFrameCounter + FAnimationSpeed * GetFrameTime;

      if (FAnimFrameCounter >= FAnims[FAnimationIndex].frameCount) and (FAnimationLoop) then
        begin
          FAnimFrameCounter:=0;
          FAnimEnded:=True;
        end
      else
      if FAnimFrameCounter >= FAnims[FAnimationIndex].frameCount then
        begin
          FAnimFrameCounter:=FAnims[FAnimationIndex].frameCount;
          FAnimEnded:=False;
        end;

       Raylib.UpdateModelAnimation(FModel, FAnims[FAnimationIndex], Round(FAnimFrameCounter));
     end;
  except
    on E: Exception do
    TraceLog(LOG_ERROR,PChar(FModelName + #32 + ' Animation is not valid') );
  end;


end;

constructor TTinyAnimatedModel.Create(ModelEngine: TTinyModelEngine);
begin
  inherited Create(ModelEngine);
  AnimationLoop:=True;
end;

procedure TTinyAnimatedModel.AssignAnim(const Value: PModelAnimation);
begin
 self.FAnims:=Value;
end;

procedure TTinyAnimatedModel.Update;
begin
///  if (Model.boneCount>0) and  then UpdateModelAnimation;
  UpdateModelAnimation;
  inherited Update;
 // if Model.boneCount>0 then UpdateModelAnimation;
end;

procedure TTinyAnimatedModel.LoadModel(FileName: String);
begin
  inherited LoadModel(FileName);
  FAnimCont:=0;
  FAnims:=LoadModelAnimations(PChar(FileName),@FAnimCont);
  FAnimationIndex:=0;
end;

procedure TTinyAnimatedModel.LoadFromModelPack(Index: integer;
  ModelPack: TTinyModelPack);
begin
  inherited LoadFromModelPack(Index, ModelPack);
  if (Index>=0) and (Index <= ModelPack.Count-1) then
    begin
      FAnimCont := ModelPack.ModelAnimCount[Index];
      FAnims := ModelPack.ModelAnimation[Index];
      if FAnimCont <= FAnimationIndex then FAnimationIndex:=0;
    end;
end;

procedure TTinyAnimatedModel.LoadFromModelPack(Name: String;
  ModelPack: TTinyModelPack);
var i: integer;
begin
  for i := 0 to Length(ModelPack.ModelName) - 1 do
  begin
    if LowerCase(Name) = LowerCase(ModelPack.ModelName[i]) then
    begin
      FModelName:= ModelPack.ModelName[i];
      FModel:= ModelPack.Model[i];
      FAnimCont := ModelPack.ModelAnimCount[i];
      FAnims := ModelPack.ModelAnimation[i];
      if FAnimCont <= FAnimationIndex then FAnimationIndex:=0;
      Exit;
    end;
  end;
end;

{ TTinyModel }
procedure TTinyModel.SetPosition(AValue: TVector3);
begin
  FPosition:=AValue;
end;

procedure TTinyModel.SetPositionX(AValue: Single);
begin
  if FPosition.x=AValue then Exit;
  FPosition.x:=AValue;
end;

procedure TTinyModel.SetPositionY(AValue: Single);
begin
   if FPosition.y=AValue then Exit;
  FPosition.y:=AValue;
end;

procedure TTinyModel.SetPositionZ(AValue: Single);
begin
  if FPosition.z=AValue then Exit;
  FPosition.z:=AValue;
end;

procedure TTinyModel.SetRotationAngle(AValue: single);
begin
  if FRotationAngle=AValue then Exit;
  FRotationAngle:=AValue;
end;

procedure TTinyModel.SetRotationAxis(AValue: TVector3);
begin
  FRotationAxis:=AValue;
end;

procedure TTinyModel.SetDrawMode(AValue: TTinyEngineDrawMode);
begin
  if FDrawMode=AValue then Exit;
  FDrawMode:=AValue;
end;



function TTinyModel.GetPositionX: Single;
begin
  result:=Fposition.x;
end;

function TTinyModel.GetPositionY: Single;
begin
  result:=Fposition.y;
end;

function TTinyModel.GetPositionZ: Single;
begin
  result:=Fposition.z;
end;

procedure TTinyModel.SetCollisionBBox(AValue: TBoundingBox);
begin
  FCollisionBBox:=AValue;
end;

procedure TTinyModel.SetCollisionMode(AValue: TTinyModelCollisionMode);
begin
  if FCollisionMode=AValue then Exit;
  FCollisionMode:=AValue;
end;

procedure TTinyModel.SetCollisionRadius(AValue: Single);
begin
  if FCollisionRadius=AValue then Exit;
  FCollisionRadius:=AValue;
end;

procedure TTinyModel.SetCollisionSphere(AValue: TVector3);
begin
  FCollisionSphere:=AValue;
end;

procedure TTinyModel.SetDebugDraw(AValue: Boolean);
begin
  if FDebugDraw=AValue then Exit;
  FDebugDraw:=AValue;
end;

procedure TTinyModel.SetScale(AValue: Single);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
end;

procedure TTinyModel.Collision(const OtherModel: TTinyModel);
var
    IsCollide: Boolean=false;
begin
  if (Collisioned and OtherModel.Collisioned) and (not FModelDead) and (not OtherModel.FModelDead) then
    begin
      // BOX > BOX
      if (self.CollisionMode = cmBBox) and (OtherModel.CollisionMode = cmBBox) then
      isCollide:= CheckCollisionBoxes(Self.FCollisionBBox,OtherModel.FCollisionBBox);

      // BOX > SPHERE
      if (self.CollisionMode = cmBBox) and (OtherModel.CollisionMode = cmSphere) then
      isCollide:= CheckCollisionBoxSphere(Self.FCollisionBBox,OtherModel.FCollisionSphere,OtherModel.FCollisionRadius);

      // SPHERE > BOX
      if (self.CollisionMode = cmSphere) and (OtherModel.CollisionMode = cmBBox) then
      isCollide:= CheckCollisionBoxSphere(OtherModel.FCollisionBBox,Self.FCollisionSphere,Self.FCollisionRadius);

      // SPHERE > SPHERE
      if (self.CollisionMode = cmSphere) and (OtherModel.CollisionMode = cmSphere) then
      isCollide:= CheckCollisionSpheres(Self.FCollisionSphere,Self.FCollisionRadius,OtherModel.FCollisionSphere,
      OtherModel.FCollisionRadius);
    end;

  if IsCollide then
     begin
       DoCollision(OtherModel);
       OtherModel.DoCollision(Self);
     end;
end;

procedure TTinyModel.Collision;
var I: Integer;
  begin
  if (FEngine <> nil) and (not FModelDead) and (FCollisioned) then
   begin
     for i := 0 to FEngine.FModelList.Count - 1 do Self.Collision(TTinyModel(FEngine.FModelList.Items[i]));
   end;
end;

procedure TTinyModel.Assign(const Value: TModel);
begin
  FModel:=Value;
end;

{$HINTS OFF}
procedure TTinyModel.DoCollision(CollisonModel: TTinyModel);
begin
  // Nothing
end;
{$HINTS ON}

constructor TTinyModel.Create(ModelEngine: TTinyModelEngine);
begin
  FEngine := ModelEngine;
  FEngine.FModelList.Add(Self);
  FModelDead:=False;
  DrawMode:=dmEx;
  Position:=Vector3Create(0.0,0.0,0.0);
  RotationAxis:=Vector3Create(0.0,0.0,0.0);
  Scale:=1.0;
  CollisionMode:=cmBBox;
  Collisioned:=false;
  CollisionRadius:=1;
  DebugDraw:=false;
  FDebugColor:= ColorCreate(0,0,255,255);
end;

destructor TTinyModel.Destroy;
begin
  UnloadTexture(Self.FTexture);
  UnloadModel(Self.FModel);
  inherited Destroy;
end;

procedure TTinyModel.Update;
var transform: TMatrix;
begin
  transform := MatrixIdentity;
  transform := MatrixMultiply(transform,MatrixRotateX(DEG2RAD*FRotationAxis.x));
  transform := MatrixMultiply(transform,MatrixRotateY(DEG2RAD*FRotationAxis.y));
  transform := MatrixMultiply(transform,MatrixRotateZ(DEG2RAD*FRotationAxis.z));
{
  transform := MatrixRotateZYX(Vector3Create(DEG2RAD*FRotationAxis.x,
                                             DEG2RAD*FRotationAxis.y,
                                             DEG2RAD*FRotationAxis.z));

 }

//  FModel.transform := MatrixIdentity;
  FModel.transform:=transform;
  { if (CollisionMode = cmSphere) then
     Vector3Set(@FCollisionSphere,FPosition.x,FPosition.y,FPosition.z);

   if (CollisionMode = cmBBox) and (FCollisionAutoSize) then
   begin
     FCollisionBBox:=GetModelBoundingBox(self.Model);
     FCollisionBBox.min:=Vector3Scale(FCollisionBBox.min,FScale);
     FCollisionBBox.max:=Vector3Scale(FCollisionBBox.max,FScale);
     FCollisionBBox.min:=Vector3Add(FCollisionBBox.min,FPosition);
     FCollisionBBox.max:=Vector3Add(FCollisionBBox.max,FPosition);
   end;  }
end;

procedure TTinyModel.Render;
  var FScaleEx: TVector3;
      FColor: TColor;
  begin
    FScaleEx:=Vector3Create(Fscale,Fscale,FScale);
    FColor:=WHITE;
    if Assigned(FEngine) then
      case FDrawMode of
        dmNormal:  DrawModel(FModel, FPosition, FScale, FColor); // Draw 3d model with texture
        dmEx:      DrawModelEx(FModel, FPosition, FRotationAxis, FRotationAngle, FScaleEx, FColor); // Draw a model with extended parameters
        dmWires:   DrawModelWires(FModel, FPosition, FScale, FColor);  // Draw a model wires (with texture if set)
        dmWiresEX: DrawModelWiresEx(FModel,FPosition,FRotationAxis, FRotationAngle, FScaleEx,FColor); // Draw a model wires with extended parameters
      end;

  if DebugDraw then
  begin
   if CollisionMode = cmBBox then DrawBoundingBox(FCollisionBBox,DebugColor);
   if CollisionMode = cmSphere then DrawSphereWires(FCollisionSphere, FCollisionRadius,8,8,DebugColor);
  end;

end;

procedure TTinyModel.Dead;
begin
    if FModelDead = False then
 begin
   FModelDead := True;
   FEngine.FModelDeadList.Add(Self);
 end;
end;

procedure TTinyModel.LoadModel(FileName: String);
begin
  FModel:=RayLib.LoadModel(Pchar(FileName));
end;

procedure TTinyModel.LoadModelTexture(TextureFileName: String;
  MaterialMap: TMaterialMapIndex);
begin
  FTexture:= LoadTexture(PChar(TextureFileName));
  SetMaterialTexture(@FModel.materials[0], MaterialMap, FTexture);// loadig material map texture
end;

procedure TTinyModel.LoadFromModelPack(Index: Integer; ModelPack: TTinyModelPack);
begin
  if (Index>=0) and (Index <= ModelPack.Count-1) then
    begin
      FModelName:= ModelPack.ModelName[Index];
      FModel := ModelPack.Model[Index];
    end;
end;

procedure TTinyModel.LoadFromModelPack(Name: String; ModelPack: TTinyModelPack);
var i: Integer;
begin
 for i := 0 to Length(ModelPack.ModelName) - 1 do
  begin
    if LowerCase(Name) = LowerCase(ModelPack.ModelName[i]) then
    begin
      FModelName:= ModelPack.ModelName[i];
      FModel := ModelPack.Model[i];
      Exit;
    end;
  end;
end;


{ TTinyModelEngine }

procedure TTinyModelEngine.SetDrawDebugGrid(AValue: boolean);
begin
  if FDrawDebugGrid=AValue then Exit;
  FDrawDebugGrid:=AValue;
end;

procedure TTinyModelEngine.SetDrawBebugModel(AValue: Boolean);
begin
  if FDrawBebugModel=AValue then Exit;
  FDrawBebugModel:=AValue;
end;

function TTinyModelEngine.GetModelsCount: Integer;
begin
   if FModelList <> nil then
     Result := FModelList.Count
  else
     Result := 0;
end;

function TTinyModelEngine.GetModel(const Index: Integer): TTinyModel;
begin
  if (FModelList <> nil) and (Index >= 0) and (Index < FModelList.Count) then
    Result := FModelList[Index]
  else
    Result := nil;
end;

procedure TTinyModelEngine.SetDrawDistance(AValue: single);
begin
  if FDrawDistance=AValue then Exit;
  FDrawDistance:=AValue;
end;

procedure TTinyModelEngine.SetDrawSkeleton(AValue: boolean);
begin
  if FDrawSkeleton=AValue then Exit;
  FDrawSkeleton:=AValue;
end;

constructor TTinyModelEngine.Create;
begin
  FModelList :=  specialize TList<TTinyModel>.Create;
  FModelDeadList := specialize TList<TTinyModel>.Create;
  DrawDistance:=0.0;
  SetDebugGrid(10,1); // set size debug grid
  FDrawSkeleton:=False;
end;

destructor TTinyModelEngine.Destroy;
  var i: integer;
begin
  for i := 0 to FModelList.Count- 1 do TTinyModel(FModelList.Items[i]).Destroy;
  FModelList.Destroy;
  FModelDeadList.Destroy;
  inherited Destroy;
end;

procedure TTinyModelEngine.Add(const Model: TTinyModel);
begin
  FModelList.Insert(FModelList.Count, Model);
end;

procedure TTinyModelEngine.Remove(const Model: TTinyModel);
begin
  FModelList.Remove(Model);
end;

procedure TTinyModelEngine.Change(Model: TTinyModel; Dest: TTinyModelEngine);
begin
  Dest.Add(Model);
  Model.Engine := Dest;
  FModelList.Remove(Model);
end;

procedure TTinyModelEngine.Update;
var i: integer;
begin
  for i := 0 to FModelList.Count - 1 do  // update all model and animation
    begin
      TTinyModel(FModelList.Items[i]).Update;
    end;
end;

procedure TTinyModelEngine.Render(Camera:TCamera);
var i: Integer;

  begin
   BeginMode3D(Camera);        // Begin 3d mode drawing

   for i:=0 to FModelList.Count-1 do
     begin
     if FDrawBebugModel then TTinyModel(FModelList.Items[i]).DebugDraw:=True
     else TTinyModel(FModelList.Items[i]).DebugDraw:=False;
     if FDrawDistance >0 then
       begin
         if Vector3Distance(Camera.Position, TTinyModel(FModelList.Items[i]).Position) <= FDrawDistance
         then TTinyModel(FModelList.Items[i]).Render;
       end
      else
        TTinyModel(FModelList.Items[i]).Render;
      end;

    if DrawDebugGrid then DrawGrid(FGridSlice, FGridSpace);

    EndMode3D();                // End 3d mode drawing, returns to orthographic 2d mode
end;


procedure TTinyModelEngine.ClearDeadModel;
  var i: Integer;
    begin
      for i := 0 to FModelDeadList.Count - 1 do
      begin
        if FModelDeadList.Count >= 0 then
        begin
          if TTinyModel(FModelDeadList.Items[i]).FModelDead = True then
          TTinyModel(FModelDeadList.Items[i]).FEngine.FModelList.Remove(FModelDeadList.Items[i]);
        end;
      end;
      FModelDeadList.Clear;
end;

procedure TTinyModelEngine.SetDebugGrid(slices: longint; spacing: single);
begin
  FGridSlice:= Slices;
  FGridSpace:= Spacing;
end;


initialization
InitCosSinTables();

end.

