unit teDescription;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms;

type
  { TTinyEApplicationDescriptor }
  TTinyApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TTinyEngineFileUnit }
  TTinyEngineFileUnit = class(TFileDescPascalUnit)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetUnitDirectives: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetInterfaceSource(const aFilename, aSourceName, aResourceName: string): string; override;
  end;

  const LE = #10; // Line End

  procedure Register;

  resourcestring
    AboutProject = 'Tiny engine game application';
    AboutDescription ='Is a set of classes for helping in the creation of 2D and 3D games in pascal.';


implementation

procedure Register;
begin
  RegisterProjectFileDescriptor(TTinyEngineFileUnit.Create,FileDescGroupName);
  RegisterProjectDescriptor(TTinyApplicationDescriptor.Create);
end;

function FileDescriptorByName() : TProjectFileDescriptor;
begin
 Result:=ProjectFileDescriptors.FindByName('TinyEngine_Unit');
end;

{ TTinyEApplicationDescriptor }
constructor TTinyApplicationDescriptor.Create;
begin
  inherited Create;
  Name:= AboutDescription;
end;

function TTinyApplicationDescriptor.GetLocalizedName: string;
begin
  Result:= AboutProject;
end;

function TTinyApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:= AboutDescription;
end;

function TTinyApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('myGame.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;


  // create program source
  NewSource:=
  'program Game1;'+LE+
   ''+LE+
  'uses'+LE+
  'Cmem, SysUtils;'+LE+
  ''+LE+
  ''+LE+
  'var Game: TGame;'+LE+
  ''+LE+
  'begin'+LE+
  '  Game:= TGame.Create;'+LE+
  '  Game.Run;'+LE+
  '  Game.Free;'+LE+
  'end.'+LE;

  AProject.MainFile.SetSourceText(NewSource,true);

  AProject.AddPackageDependency('ray4laz');
  AProject.AddPackageDependency('TinyEngine');
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='Game';
end;

function TTinyApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoNewEditorFile(FileDescriptorByName,'','',[nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

{ TTinyEngineFileUnit }
constructor TTinyEngineFileUnit.Create;
begin
  inherited Create;
  Name:='TinyEngine_Unit';
  UseCreateFormStatements:=False;
end;

function TTinyEngineFileUnit.GetInterfaceUsesSection: string;
begin
  Result:='Raylib, teApplication'
end;

function TTinyEngineFileUnit.GetUnitDirectives: string;
begin
  Result := '{$mode objfpc}{$H+} '
end;

function TTinyEngineFileUnit.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:=
  'constructor TGame.Create;'+LE+
  'begin'+LE+

  'end;'+LE+
  ''+LE+
  'procedure TGame.Init;'+LE+
  'begin'+LE+
  '  inherited Init;'+LE+
  ' InitWindow(800, 600, ''raylib [Game Project]'');'+LE+
  ' SetWindowState(FLAG_VSYNC_HINT or FLAG_MSAA_4X_HINT);'+LE+
  ' SetTargetFPS(60);' +LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Update;'+LE+
  'begin'+LE+
  '  inherited Update;'+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Render;'+LE+
  'begin'+LE+
  '  inherited Render;'+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Resized;'+LE+
  'begin'+LE+
  '  inherited Resized;'+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Shutdown;'+LE+
  'begin'+LE+
  '  inherited Shutdown;'+LE+
  'end;' +LE+LE;

end;

function TTinyEngineFileUnit.GetInterfaceSource(const aFilename, aSourceName,
  aResourceName: string): string;
begin
  Result:=
  'type'+LE+
  'TGame = class(TTinyApplication)'+LE+
  '  private'+LE+
  '  protected'+LE+
  '  public'+LE+
  '    constructor Create; override;'+LE+
  '    procedure Init; override;'+LE+
  '    procedure Update; override;'+LE+
  '    procedure Render; override;'+LE+
  '    procedure Shutdown; override;'+LE+
  '    procedure Resized; override;'+LE+
  '  end;'+LE+LE
end;

end.

