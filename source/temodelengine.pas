unit teModelEngine;

{$mode objfpc}{$H+}

interface

uses
  raylib, raymath, rlgl,
  Classes, SysUtils, Generics.Collections;

type
  TTinyEngineDrawMode = (dmNormal, dmEx, dmWires, dmWiresEx); // draw model mode

  TTinyModel = class;

  { TTinyModelEngine }
  TTinyModelEngine = class
    private
      FDrawDebugGrid: boolean;
      FDrawDistance: single;
      FDrawSkeleton: boolean;
      FGridSlice: longint;
      FGridSpace: single;

      procedure SetDrawDebugGrid(AValue: boolean);
      procedure SetDrawDistance(AValue: single);
      procedure SetDrawSkeleton(AValue: boolean);
    protected
      FModelList: specialize TList<TTinyModel>; // list of model
      FModelDeadList: specialize TList<TTinyModel>; // model dead list
    public
      constructor Create;
      destructor Destroy; override;
      procedure Update;  // update engine
      procedure Render(Camera: TCamera); virtual; // render engine
      procedure ClearDeadModel;  // clear of death model
      procedure SetDebugGrid(slices:longint; spacing:single); // set grid size and slice
    published
      property DrawSkeleton: boolean read FDrawSkeleton write SetDrawSkeleton;
      property DrawDebugGrid: boolean read FDrawDebugGrid write SetDrawDebugGrid;
      property DrawDistance: single read FDrawDistance write SetDrawDistance;
  end;

  { TrlModel }
  TTinyModel = class
    private
      FDrawMode: TTinyEngineDrawMode;
      FPosition: TVector3;
      FRotationAngle: single;
      FRotationAxis: TVector3;
      FScale: Single;
      function GetPositionX: Single;
      function GetPositionY: Single;
      function GetPositionZ: Single;
      procedure SetDrawMode(AValue: TTinyEngineDrawMode);
      procedure SetPosition(AValue: TVector3);
      procedure SetPositionX(AValue: Single);
      procedure SetPositionY(AValue: Single);
      procedure SetPositionZ(AValue: Single);
      procedure SetRotationAngle(AValue: single);
      procedure SetRotationAxis(AValue: TVector3);
      procedure SetScale(AValue: Single);
    protected
      FModelDead: Boolean;
      FEngine: TTinyModelEngine;
      FModel: TModel;
      FTexture: TTexture;
      FAnimCount: integer;
    public
      constructor Create(Engine: TTinyModelEngine); virtual;
      destructor Destroy; override;
      procedure Update; overload; virtual;
      procedure Render; virtual;
      procedure Dead;
      procedure LoadModel(FileName: string); virtual;


      property DrawMode: TTinyEngineDrawMode read FDrawMode write SetDrawMode;
      property Model: TModel read FModel write FModel;
      property RotationAxis: TVector3 read FRotationAxis write SetRotationAxis;
      property Position: TVector3 read FPosition write SetPosition;
      property PositionX: Single read GetPositionX write SetPositionX;
      property PositionY: Single read GetPositionY write SetPositionY;
      property PositionZ: Single read GetPositionZ write SetPositionZ;
      property RotationAngle: single read FRotationAngle write SetRotationAngle;


      property Scale: Single read FScale write SetScale;
  end;

implementation

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

procedure TTinyModel.SetScale(AValue: Single);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
end;

constructor TTinyModel.Create(Engine: TTinyModelEngine);
begin
  FEngine := Engine;
  FEngine.FModelList.Add(Self);
  FModelDead:=False;
  DrawMode:=dmEx;
  Position:=Vector3Create(0.0,0.0,0.0);
  RotationAxis:=Vector3Create(0.0,0.0,0.0);
  Scale:=1.0;
  FAnimCount:=0;
//  CollisionMode:=cmBBox;
//  CollisionRadius:=1;
//  Collisioned:=false;
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
    //FScaleEx:=Vector3Create(1,1,1);
    FColor:=WHITE;
    if Assigned(FEngine) then
      case FDrawMode of
        dmNormal: DrawModel(FModel, FPosition, FScale, WHITE); // Draw 3d model with texture
        dmEx: DrawModelEx(FModel, FPosition, FRotationAxis, FRotationAngle, FScaleEx, FColor); // Draw a model with extended parameters
        dmWires: DrawModelWires(FModel, FPosition, FScale, FColor);  // Draw a model wires (with texture if set)
        dmWiresEX: DrawModelWiresEx(FModel,FPosition,FRotationAxis, FRotationAngle, FScaleEx,FColor); // Draw a model wires with extended parameters
      end;
   // DrawBoundingBox(FCollisionBBox,RED)

end;

procedure TTinyModel.Dead;
begin
    if FModelDead = False then
 begin
   FModelDead := True;
   FEngine.FModelDeadList.Add(Self);
 end;
end;

procedure TTinyModel.LoadModel(FileName: string);
begin
FModel:=RayLib.LoadModel(Pchar(FileName));
end;


{ TTinyModelEngine }

procedure TTinyModelEngine.SetDrawDebugGrid(AValue: boolean);
begin
  if FDrawDebugGrid=AValue then Exit;
  FDrawDebugGrid:=AValue;
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

procedure TTinyModelEngine.Update;
  var i: integer;
      matScale: TMatrix;
  begin
//  UpdateCamera(@FCamera);
   for i := 0 to FModelList.Count - 1 do  // update all model and animation
      begin
       TTinyModel(FModelList.Items[i]).Update;
      end;
end;

procedure TTinyModelEngine.Render(Camera:TCamera);
  var i,j: longint;
      ModVec:TVector3;//(Vector3Subtract
  begin
   BeginMode3D(Camera);        // Begin 3d mode drawing

   for i:=0 to FModelList.Count-1 do
     begin
      if FDrawDistance >0 then
       begin
         if Vector3Distance(Camera.Position, TTinyModel(FModelList.Items[i]).Position) <= FDrawDistance
         then TTinyModel(FModelList.Items[i]).Render;
       end
      else
        TTinyModel(FModelList.Items[i]).Render;



      for j := 0 to TTinyModel(FModelList.Items[i]).Model.boneCount - 1 do
      begin
        if {(not animPlaying) or }(TTinyModel(FModelList.Items[i]).FAnimCount<=0) then
               begin
               ModVec:=TTinyModel(FModelList.Items[i]).model.bindPose[j].translation;



               // Display the bind-pose skeleton

               DrawCube(Vector3Add(ModVec, TTinyModel(FModelList.Items[i]).FPosition)
               , 0.02, 0.02, 0.02, RED);

               if (TTinyModel(FModelList.Items[i]).model.bones[j].parent >= 0) then
                 begin
                   DrawLine3D(ModVec,
                   TTinyModel(FModelList.Items[i]).model.bindPose[TTinyModel(FModelList.Items[i]).model.bones[j].parent].translation, blue);
                 end;
               end
        end;
      end;


   if DrawDebugGrid then DrawGrid(FGridSlice, FGridSpace);

   EndMode3D();                // End 3d mode drawing, returns to orthographic 2d mode
end;


procedure TTinyModelEngine.ClearDeadModel;
  var i: Integer;
    begin
      for i := 0 to FModelDeadList.Count - 1 do
      begin
        if FModelDeadList.Count >= 1 then
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

end.

