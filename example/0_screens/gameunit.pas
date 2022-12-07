unit gameUnit;

{$mode objfpc}{$H+}

interface

uses
  cmem, raylib, teApplication;

type
TGame = class(TTinyApplication)
  private
    Texture1: TTexture2D;
    Texture2: TTexture2D;
  protected
    Screens: TTinyGameScreens;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
    procedure Resized; override;
  end;

{ TMainScreen }
TMainScreen =  class(TTinyGameScreen)
  private
    Owner_: TGame;
  public
    constructor Create(AOwner: TTinyGameScreens; AData: Pointer); override;
    procedure Update; override;
    procedure Render; override;
  end;

{ TMenuScreen }
TMenuScreen =  class(TTinyGameScreen)
  private
    Owner_: TGame;
  public
    constructor Create(AOwner: TTinyGameScreens; AData: Pointer); override;
    procedure Update; override;
    procedure Render; override;
  end;

implementation

{ TMenuScreen }

constructor TMenuScreen.Create(AOwner: TTinyGameScreens; AData: Pointer);
begin
  inherited Create(AOwner, AData);
  Owner_:= TGame(Data);
end;

procedure TMenuScreen.Update;
begin
  if IsKeyReleased(KEY_ESCAPE) then
  begin
     Open('Main');
     Close;
  end;
end;

procedure TMenuScreen.Render;
begin
  DrawTexture(Owner_.Texture2,0,0,WHITE);
end;

{ TMainScreen }

constructor TMainScreen.Create(AOwner: TTinyGameScreens; AData: Pointer);
begin
  inherited Create(AOwner, AData);
  Owner_:= TGame(Data);
end;

procedure TMainScreen.Update;
begin
  if IsKeyReleased(KEY_ESCAPE) then
  begin
  Open('Menu');
  Close;
  end;
end;

procedure TMainScreen.Render;
begin
  DrawTexture(Owner_.Texture1,0,0,WHITE);
end;

constructor TGame.Create;
begin
end;

procedure TGame.Init;
begin
  inherited Init;
  InitWindow(800, 600, 'raylib [Screens demo]');
  SetWindowState(FLAG_VSYNC_HINT or FLAG_MSAA_4X_HINT);
  //SetTargetFPS(60);

  // Disable esc to exit
  SetExitKey(0);


   // Texture loading
  Texture1 := LoadTexture('data/image/Background0.png');
  Texture2 := LoadTexture('data/image/Background1.png');


  Screens:= TTinyGameScreens.Create;
  Screens.Add(TMainScreen, 'Main', Self);
  Screens.Add(TMenuScreen, 'Menu', Self);

  Screens.Open('Main');
end;

procedure TGame.Update;
begin
  inherited Update;
  Screens.Update(GetFrameTime);

end;

procedure TGame.Render;
begin
  Screens.Render;
   if Assigned(Screens.Active) then
  begin
   DrawText(PChar('Active screen: ' + Screens.Active.Name),10,20,10,SKYBLUE);
  end else
  begin
    DrawText(PChar('Active screen: null'),10,20,10,BLACK);
  end;
    DrawText(PChar('Press esc to change screen render'),10,10,10,BLACK);

   DrawFPS(70,10);
end;

procedure TGame.Resized;
begin
  inherited Resized;
end;

procedure TGame.Shutdown;
begin
  // Texture unloading
  UnloadTexture(texture1);
  UnloadTexture(texture2);
  inherited Shutdown;
end;

end.

