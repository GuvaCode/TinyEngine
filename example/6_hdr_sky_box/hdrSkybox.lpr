program hdrSkybox;

uses
    SysUtils, hdr;


var Game: TGame;

begin
  Game:= TGame.Create;
  Game.Run;
  Game.Free;
end.
