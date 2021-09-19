unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Media,
  FMX.StdCtrls, Winsoft.FireMonkey.Obr, FMX.Layouts, FMX.ExtCtrls, System.Actions,
  FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Memo,
  FMX.Controls.Presentation, FMX.ScrollBox;

type
  TFormMain = class(TForm)
    ButtonTakePhoto: TButton;
    FObr: TFObr;
    ActionList: TActionList;
    TakePhotoFromCameraAction: TTakePhotoFromCameraAction;
    Memo: TMemo;
    ImageControl: TImageControl;
    procedure TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
    procedure TakePhotoFromCameraActionDidCancelTaking;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.TakePhotoFromCameraActionDidCancelTaking;
begin
  Memo.Lines.Clear;
end;

procedure TFormMain.TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
var
  i: Integer;
  Barcode: TObrSymbol;
begin
  Memo.Lines.Clear;

  ImageControl.Bitmap.Assign(Image);

  FObr.Active := True;
  FObr.Picture.Assign(Image);
  FObr.Scan;
  if FObr.BarcodeCount = 0 then
    Memo.Lines.Append('No barcode found')
  else
    for i := 0 to FObr.BarcodeCount - 1 do
    begin
      Barcode := FObr.Barcode[i];
      Memo.Lines.Append(Barcode.SymbologyName + Barcode.SymbologyAddonName + ' ' + Barcode.OrientationName + ' ' + Barcode.DataUtf8);
    end;
end;

end.
