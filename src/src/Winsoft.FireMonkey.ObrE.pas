//---------------------------------------------------------------------
//
// Optical Barcode Recognision Component for FireMonkey
//
// Copyright (c) 2013-2018 WINSOFT
//
//---------------------------------------------------------------------

unit Winsoft.FireMonkey.ObrE;

interface

procedure Register;

implementation

uses DesignEditors, DesignIntf, System.Classes, Winsoft.FireMonkey.Obr;

procedure Register;
begin
  RegisterComponents('System', [TFObr]);
end;

end.