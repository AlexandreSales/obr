unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Winsoft.FireMonkey.Obr, FMX.Layouts, FMX.Memo, FMX.ExtCtrls, FMX.Media,
  FMX.Objects, FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    FObr: TFObr;
    PanelTop: TPanel;
    ButtonStartStop: TButton;
    PanelBottom: TPanel;
    Memo: TMemo;
    CameraComponent: TCameraComponent;
    Image: TImage;
    ScrollBox: TScrollBox;
    procedure ButtonStartStopClick(Sender: TObject);
    procedure FObrBarcodeDetected(Sender: TObject);
    procedure CameraComponentSampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
  private
    { Private declarations }
    procedure Start;
    procedure Stop;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.Start;
begin
  FObr.Active := True;
  CameraComponent.Active := True;
  ButtonStartStop.Text := 'Stop';
end;

procedure TFormMain.Stop;
begin
  CameraComponent.Active := False;
  FObr.Active := False;
  ButtonStartStop.Text := 'Start';
end;

procedure TFormMain.ButtonStartStopClick(Sender: TObject);
begin
  try
    if not FObr.Active then
      Start
    else
      Stop
  except
    on E: Exception do
    begin
      Stop;
      raise
    end;
  end
end;

procedure TFormMain.CameraComponentSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
  CameraComponent.SampleBufferToBitmap(Image.Bitmap, True);
  CameraComponent.SampleBufferToBitmap(FObr.Picture, True);
  FObr.Scan;
end;

procedure TFormMain.FObrBarcodeDetected(Sender: TObject);
var
  i: Integer;
  Barcode: TObrSymbol;
  Line: string;
begin
  for i := 0 to FObr.BarcodeCount - 1 do
  begin
    Barcode := FObr.Barcode[i];
    Line := Barcode.SymbologyName + Barcode.SymbologyAddonName + ' ' + Barcode.OrientationName + ' ' + Barcode.DataUtf8;
    if i < Memo.Lines.Count  then
      Memo.Lines[i] := Line
    else
      Memo.Lines.Add(Line);
  end;

  while Memo.Lines.Count > FObr.BarcodeCount do
    Memo.Lines.Delete(Memo.Lines.Count - 1);
end;

end.
