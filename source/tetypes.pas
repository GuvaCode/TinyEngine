unit teTypes;

{$mode objfpc}{$H+}

interface

uses
  RayLib, Classes, SysUtils;

type

//------------------------------------------------------------------------------
TShiftState = (
  ssCtrl,// The control key is pressed
  ssAlt,// The alt key is pressed
  ssShift,// The shift key is pressed
  ssCaps// The caps lock key is pressed
);

// Set of shift states
TTEShiftStates = set of TTEShiftState;



implementation

end.

