unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Winsoft.FireMonkey.Obr, FMX.Layouts, FMX.Memo, FMX.ExtCtrls, FMX.ScrollBox,
  FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    FObr: TFObr;
    PanelTop: TPanel;
    ButtonSelectPicture: TButton;
    PanelBottom: TPanel;
    Memo: TMemo;
    OpenDialog: TOpenDialog;
    ImageViewer: TImageViewer;
    procedure ButtonSelectPictureClick(Sender: TObject);
    procedure FObrBarcodeDetected(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonSelectPictureClick(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
    begin
      Memo.Lines.Clear;
      ImageViewer.Bitmap.LoadFromFile(FileName);

      FObr.Active := True;
      try
        FObr.Picture.Assign(ImageViewer.Bitmap);
        FObr.Scan;
        if FObr.BarcodeCount = 0 then
          Memo.Lines.Append('No barcode found')
      finally
        FObr.Active := False;
        FObr.Picture := nil;
      end;
    end
end;

procedure TFormMain.FObrBarcodeDetected(Sender: TObject);
var
  i: Integer;
  Barcode: TObrSymbol;
begin
  Memo.Lines.Clear;
  for i := 0 to FObr.BarcodeCount - 1 do
  begin
    Barcode := FObr.Barcode[i];
    Memo.Lines.Append(Barcode.SymbologyName + Barcode.SymbologyAddonName + ' ' + Barcode.OrientationName + ' ' + Barcode.DataUtf8);
  end;
end;

end.
