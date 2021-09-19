unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Media,
  FMX.StdCtrls, Winsoft.FireMonkey.Obr, FMX.Layouts, FMX.ExtCtrls, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Memo, FMX.ScrollBox,
  FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    ToolBar: TToolBar;
    ButtonScan: TButton;
    FObr: TFObr;
    Memo: TMemo;
    ImageControl: TImageControl;
    procedure ButtonScanClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonScanClick(Sender: TObject);
var
  I: Integer;
  BarCode: TObrSymbol;
begin
  FObr.Active := True;
  FObr.Picture.Assign(ImageControl.Bitmap);
  FObr.Scan;
  if FObr.BarcodeCount = 0 then
    Memo.Lines.Append('No barcode found')
  else
    for I := 0 to FObr.BarcodeCount - 1 do
    begin
      Barcode := FObr.Barcode[I];
      Memo.Lines.Append(Barcode.SymbologyName + Barcode.SymbologyAddonName + ' ' + Barcode.OrientationName + ' ' + Barcode.DataUtf8);
      Memo.GoToTextEnd;
    end;
end;

end.