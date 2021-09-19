unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Media, FMX.Layouts, FMX.ExtCtrls, FMX.Memo, FMX.Objects,
  Winsoft.FireMonkey.Obr, FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox;

type
  TFormMain = class(TForm)
    ButtonStartStop: TButton;
    CameraComponent: TCameraComponent;
    Memo: TMemo;
    FObr: TFObr;
    ImageViewer: TImageViewer;
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

uses System.IOUtils;

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

procedure TFormMain.CameraComponentSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
  CameraComponent.SampleBufferToBitmap(ImageViewer.Bitmap, True);
  CameraComponent.SampleBufferToBitmap(FObr.Picture, True);
  FObr.Scan;
end;

procedure TFormMain.FObrBarcodeDetected(Sender: TObject);
var
  i: Integer;
  Barcode: TObrSymbol;
begin
  for i := 0 to FObr.BarcodeCount - 1 do
  begin
    Barcode := FObr.Barcode[i];
    Memo.Lines.Append(Barcode.SymbologyName + Barcode.SymbologyAddonName + ' ' + Barcode.OrientationName + ' ' + Barcode.DataUtf8);
    Memo.GoToTextEnd;
  end;
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

end.
