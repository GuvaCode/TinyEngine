unit teApplication;

{$mode objfpc}{$H+}

interface

uses
  Raylib, Classes, Types, SysUtils, Generics.Collections;

type
// Template class for the user application
 { TTinyApplication }
 TTinyApplication = class(TObject)
  private
    FClearBackgroundColor: TColorB;
    Terminated: Boolean;
  protected
    // Return the path containing the content for the application
    function GetContentPath: String; virtual;
  public
    // Create a new application
    constructor Create; virtual;
    // Free the application
    destructor Destroy; override;

    // Initializes the application
    procedure Init; virtual;
    // Shutdown the application
    procedure Shutdown; virtual;
    // Update the application
    procedure Update; virtual;
    // Render the application
    procedure Render; virtual;
    // Called when the device is resized
    procedure Resized; virtual;
    // Run the application
    procedure Run;
    // Terminate the application
    procedure Terminate;
    // Return the path containing the content for the application
    property ContentPath: String read GetContentPath;
    // Render background for application
    property ClearBackgroundColor: TColorB read FClearBackgroundColor write FClearBackgroundColor;
 end;

 TTinyGameScreen  = class;
 TTinyGameScreens = class;

 TTinyTransitionMode = (tmNone, tmShow, tmHide);

 // Controls transitions when opening and closing screens
 { TTinyGameTransition }
 TTinyGameTransition = class
  private
    FOwner: TTinyGameScreen;

    FMode    : TTinyTransitionMode;
    FTime    : Single;
    FPosition: Single;
    FDone    : Boolean;

    FTimeShow: Single;
    FTimeHide: Single;
  public
    constructor Create(AOwner: TTinyGameScreen);
    destructor Destroy; override;

    // Reset the transition
    procedure Reset(Mode: TTinyTransitionMode);
    // Update the transition
    procedure Update(const FrameTime: Single);

    property Mode: TTinyTransitionMode read FMode write FMode;
    // Current transition time in seconds
    property Time: Single read FTime write FTime;
    // Current transition position ranging from 0.0 (Hidden) to 1.0 (Visible)
    property Position: Single read FPosition write FPosition;
    // The transition is completed
    property Done: Boolean read FDone;

    // How long the screen takes to transition on when it is activated.
    property TimeShow: Single read FTimeShow write FTimeShow;
    // How long the screen takes to transition off when it is deactivated.
    property TimeHide: Single read FTimeHide write FTimeHide;
  end;

 TTinyGameScreenClass = class of TTinyGameScreen;

 // A game screen is
 { TTinyGameScreen }
 TTinyGameScreen = class(TPersistent)
  private
    FOwner     : TTinyGameScreens;
    FName      : String;
    FData      : Pointer;
    FTransition: TTinyGameTransition;
  protected
    // Celled when the screen is opened
    procedure Opened; virtual;
    // Celled when the screen is closed
    procedure Closed; virtual;
  public
    constructor Create(AOwner: TTinyGameScreens; AData: Pointer); virtual;
    destructor Destroy; override;

    // Update the game screen
    procedure Update; virtual;
    // Render the game screen
    procedure Render; virtual;

    // Open this screen
    procedure Open; overload;
    // Open another screen
    procedure Open(const Name: String); overload;
    // Close this screen
    procedure Close;

    // The owning screen manager
    property Owner: TTinyGameScreens read FOwner;
    // Name of the screen
    property Name: String read FName write FName;
    // Custom data
    property Data: Pointer read FData write FData;
    // Screen transition
    property Transition: TTinyGameTransition read FTransition;
  end;

 // Manager for game screens
 { TTinyGameScreens }
 TTinyGameScreens = class
  private
    // List of all known screens
    FScreens: specialize TList<TTinyGameScreen>;
    // List of the current active screens,
    // The top most scene recieves the update event, and all scenes the render
    FStack: specialize TList<TTinyGameScreen>;
    // List of screens that is transitioning
   FTransitions: specialize TList<TTinyGameScreen>;
    function GetActive: TTinyGameScreen;
  protected
    property Stack : specialize TList<TTinyGameScreen> read FStack;
    // List of screens that is transitioning
    property Transitions : specialize TList<TTinyGameScreen> read FTransitions;
  public
    // Creates a screen manager
    constructor Create;
    // Default destructor
    destructor Destroy; override;

    // Remove and free all screens
    procedure Clear;

    // Add a screen to the manager
    procedure Add(Screen: TTinyGameScreen); overload;
    // Add a screen to the manager
    procedure Add(Screen: TTinyGameScreenClass; const Name: String; const Data: Pointer); overload;

    // Open a game screen
    procedure Open(Screen: TTinyGameScreen); overload;
    // Close a game screen
    procedure Close(Screen: TTinyGameScreen); overload;

    // Open a game screen
    procedure Open(const Name: String); overload;
    // Close a game screen
    procedure Close(const Name: String); overload;

    // Update the active screen
    procedure Update(const FrameTime: Single);
    // Renders all visible screens
    procedure Render;

    // Find a screen by name
    function FindScreen(const Name: String): TTinyGameScreen;

    // Detemines if a screen is visible
    function IsVisible(const Screen: TTinyGameScreen): Boolean; overload;
    // Determines if a screen is active
    function IsActive(const Screen: TTinyGameScreen): Boolean; overload;

    // List of game screens
    property Screens: specialize TList<TTinyGameScreen> read FScreens;
    // The active screens
    property Active: TTinyGameScreen read GetActive;
  end;


implementation

//------------------------------------------------------------------------------
{ TTinyGameScreens }
function TTinyGameScreens.GetActive: TTinyGameScreen;
begin
 if Stack.Count > 0 then
 begin
   Result:= TTinyGameScreen( Stack.Last );
 end else
 begin
   Result:= nil;
 end;
end;

constructor TTinyGameScreens.Create;
begin
  FScreens     := specialize TList<TTinyGameScreen>.Create;
  FStack       := specialize TList<TTinyGameScreen>.Create;
  FTransitions := specialize TList<TTinyGameScreen>.Create;
end;

destructor TTinyGameScreens.Destroy;
begin
  Clear;
  FScreens.Free;
  FStack.Free;
  FTransitions.Free;
  inherited Destroy;
end;

procedure TTinyGameScreens.Clear;
var Screen: TTinyGameScreen;
    Index: Integer;
begin
  for Index := 0 to FScreens.Count - 1 do
   begin
     Screen:=TTinyGameScreen(FScreens[Index]);
     Screen.Free;
   end;
  FScreens.Clear;
  FStack.Clear;
  FTransitions.Clear;
end;

procedure TTinyGameScreens.Add(Screen: TTinyGameScreen);
begin
  FScreens.Add(Screen);
end;

procedure TTinyGameScreens.Add(Screen: TTinyGameScreenClass; const Name: String;
  const Data: Pointer);
var AScreen: TTinyGameScreen;
begin
  AScreen:= Screen.Create(Self, Data);
  AScreen.Name:= Name;
  Add(AScreen);
end;

procedure TTinyGameScreens.Open(Screen: TTinyGameScreen);
begin
  // The screen is already opened
  if Stack.IndexOf(Screen) >= 0 then Exit;
  // The screen is already opened
  if Transitions.IndexOf(Screen) >= 0 then Exit;

  if Screen.Transition.TimeShow > 0 then
  begin
    FTransitions.Add(Screen);
    Screen.Transition.Reset(tmShow);
  end;

  FStack.Add(Screen);
  Screen.Opened;
end;

procedure TTinyGameScreens.Close(Screen: TTinyGameScreen);
begin
  // The screen is already opened
  if Transitions.IndexOf(Screen) >= 0 then Exit;
  if Screen.Transition.TimeHide > 0 then
  begin
    Screen.Transition.Reset(tmHide);
    FTransitions.Add(Screen);
  end else
  begin
    Screen.Closed;
    FStack.Remove(Screen);
  end;
end;

procedure TTinyGameScreens.Open(const Name: String);
var Screen: TTinyGameScreen;
begin
  Screen:= FindScreen(Name);
  if Assigned(Screen) then
  begin
    Open(Screen);
  end else
  begin
      TraceLog(LOG_ERROR,PChar(Format('The game screen "%s" was not found', [Name])));
      raise Exception.CreateFmt('The game screen "%s" was not found', [Name]);
  end;
end;

procedure TTinyGameScreens.Close(const Name: String);
var Screen: TTinyGameScreen;
begin
  Screen:= FindScreen(Name);
  if Assigned(Screen) then
  begin
    Close(Screen);
  end else
  begin
    TraceLog(LOG_ERROR,PChar(Format('The game screen "%s" was not found', [Name])));
    raise Exception.CreateFmt('The game screen "%s" was not found', [Name]);
  end;
end;

procedure TTinyGameScreens.Update(const FrameTime: Single);
var Index : Integer;
    Screen: TTinyGameScreen;
begin
  Index:= 0;
  while Index < Transitions.Count do
  begin
    Screen:= TTinyGameScreen(Transitions[Index]);
    // Update the transition
    Screen.Transition.Update(FrameTime);
    // When the transition is completed, remove the screen from the transition list
    if Screen.Transition.Done then
    begin
      FTransitions.Delete(Index);
      if Screen.Transition.Mode = tmHide then
      begin
        Screen.Closed;
        FStack.Remove(Screen);
      end;
    end else
    begin
      Inc(Index);
    end;
  end;
  // Update the screen on top of the stack
  if Stack.Count > 0 then
  begin
    Screen:= TTinyGameScreen(FStack.Last);
    Screen.Update;
  end;
end;

procedure TTinyGameScreens.Render;
var Index: Integer;
    Item : TTinyGameScreen;
begin
  for Index := 0 to Stack.Count - 1 do
  begin
    Item:= TTinyGameScreen(Stack[Index]);
    Item.Render;
  end;
end;

function TTinyGameScreens.FindScreen(const Name: String):TTinyGameScreen;
var Index: Integer;
    Item : TTinyGameScreen;
begin
  for Index := 0 to FScreens.Count - 1 do
  begin
    Item:= TTinyGameScreen(FScreens[Index]);
    if SameText(Item.Name, Name) then
    begin
      Result:= Item;
      Exit;
    end;
  end;
  Result:= nil;
end;

function TTinyGameScreens.IsVisible(const Screen: TTinyGameScreen): Boolean;
var Index  : Integer;
    Current: TTinyGameScreen;
begin
  for Index := 0 to FStack.Count - 1 do
  begin
    Current:= TTinyGameScreen(Stack[Index]);
    if Current = Screen then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Result:= False;
end;

function TTinyGameScreens.IsActive(const Screen: TTinyGameScreen): Boolean;
begin
  if Stack.Count > 0 then
  begin
    Result:= TTinyGameScreen(FStack.Last) = Screen;
  end else
  begin
    Result:= False;
  end;
end;

{ TTinyGameScreen }

procedure TTinyGameScreen.Opened;
begin

end;

procedure TTinyGameScreen.Closed;
begin

end;

constructor TTinyGameScreen.Create(AOwner: TTinyGameScreens; AData: Pointer);
begin
 FOwner := AOwner;
 FData  := AData;
 FName  := '';
 FTransition:= TTinyGameTransition.Create(Self);
end;

destructor TTinyGameScreen.Destroy;
begin
  FTransition.Free;
  inherited Destroy;
end;

procedure TTinyGameScreen.Update;
begin

end;

procedure TTinyGameScreen.Render;
begin

end;

procedure TTinyGameScreen.Open;
begin
  FOwner.Open(Self);
end;

procedure TTinyGameScreen.Open(const Name: String);
begin
  FOwner.Open(Name);
end;

procedure TTinyGameScreen.Close;
begin
  FOwner.Close(Self);
end;

{ TTinyGameTransition }
constructor TTinyGameTransition.Create(AOwner: TTinyGameScreen);
begin
  FOwner := AOwner;

  FMode    := tmNone;
  FTime    := 0;
  FPosition:= 0.0;
  FDone    := True;
  FTimeShow:= 1.0;
  FTimeHide:= 1.0;
end;

destructor TTinyGameTransition.Destroy;
begin
  inherited Destroy;
end;

procedure TTinyGameTransition.Reset(Mode: TTinyTransitionMode);
begin
 FMode    := Mode;
 FDone    := False;
 FTime    := 0;
 FPosition:= 0.0;
end;

procedure TTinyGameTransition.Update(const FrameTime: Single);
begin
 // No transition is acitve
 if Done or (Mode = tmNone) then Exit;

 // Show transition
 if Mode = tmShow then
 begin
   FTime:= FTime + FrameTime;
   FPosition:= (FTime / FTimeShow);

   if FPosition >= 0.1 then
   begin
     FPosition:= 0.1;
     FDone:= True;
   end;
 end else

 // Hide transition
 if Mode = tmHide then
 begin
   FTime:= FTime + FrameTime;
   FPosition:= 0.1 - (FTime / FTimeHide);

   if FPosition <= 0.0 then
   begin
     FPosition:= 0.0;
     FDone:= True;
   end;
 end;
end;

{ TTinyApplication }
function TTinyApplication.GetContentPath: String;
begin
 result:=GetWorkingDirectory;
end;

constructor TTinyApplication.Create;
begin

end;

destructor TTinyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TTinyApplication.Init;
begin
  FClearBackgroundColor := WHITE;
end;

procedure TTinyApplication.Shutdown;
begin
  CloseWindow(); // Close window and OpenGL context
end;

procedure TTinyApplication.Update;
begin
  Terminated:=WindowShouldClose;
  if IsWindowResized then Resized;
end;

procedure TTinyApplication.Render;
begin

end;

procedure TTinyApplication.Resized;
begin

end;

procedure TTinyApplication.Run;
begin
  Terminated:= False;
  Init;
  // Enter the main loop
  while not Terminated do
  begin
    Update;
    BeginDrawing;
    ClearBackground(FClearBackgroundColor);
    Render;
    EndDrawing;
  end;

  Shutdown;
end;

procedure TTinyApplication.Terminate;
begin
 Terminated:= True;
end;



end.


