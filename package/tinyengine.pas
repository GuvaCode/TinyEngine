{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TinyEngine;

{$warn 5023 off : no warning about unused units}
interface

uses
  teDescription, teApplication, teModelEngine, teLights, teTPCamera, 
  teFreeCamera, teFPCamera, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('teDescription', @teDescription.Register);
end;

initialization
  RegisterPackage('TinyEngine', @Register);
end.
