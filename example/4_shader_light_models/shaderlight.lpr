program shaderlight;

uses
    cmem, SysUtils, munit;


var Game: TGame;

begin
  Game:= TGame.Create;
  Game.Run;
  Game.Free;
end.
