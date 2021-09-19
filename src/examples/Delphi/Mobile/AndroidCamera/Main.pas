unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ExtCtrls, FMX.StdCtrls, Winsoft.Android.Camera, Androidapi.JNI.GraphicsContentViewText,
  FMX.Platform, FMX.Objects, FMX.Controls.Presentation, Winsoft.FireMonkey.Obr;

type
  TFormMain = class(TForm)
    ACamera: TACamera;
    ButtonSettings: TButton;
    ButtonStartStop: TButton;
    Image: TImage;
    LabelBarcode: TLabel;
    Rectangle: TRectangle;
    procedure ACameraDataReady(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FMXApplicationEventService: IFMXApplicationEventService;
    Scale: Single;
    Paused: Boolean;
    RgbaDataWidth: Integer;
    RgbaDataHeight: Integer;
    RgbaData: array of Cardinal;
    Scanning: Boolean;
    ScanBitmap: TBitmap;
    ScanBarcode: string;
    ScanRectangle: TRectF;
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  public
    { Public declarations }
    procedure Start;
    procedure Stop;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses Settings;

var
  Processing: Integer;

procedure TFormMain.ACameraDataReady(Sender: TObject);
begin
  if Processing = 0 then
  begin
    Inc(Processing);
    ACamera.DataToRgba(@RgbaData[0], ACamera.PreviewRotation, ACamera.PreviewHorizMirror);

    TThread.Queue(nil,
      procedure
      begin
        try
          try
            if (ACamera.PreviewRotation = ro0) or (ACamera.PreviewRotation = ro180) then
              ACamera.UpdateBitmap(Image.Bitmap, @RgbaData[0], RgbaDataWidth, RgbaDataHeight)
            else
              ACamera.UpdateBitmap(Image.Bitmap, @RgbaData[0], RgbaDataHeight, RgbaDataWidth);

            if not Scanning then
            begin
              if ScanBarcode <> '' then
                LabelBarcode.Text := ScanBarcode;

              if ScanRectangle.Size <> TPointF.Zero then
              begin
                Rectangle.Position.Point := ScanRectangle.Location + TPointF.Create((Image.Width - Image.Bitmap.Width / Scale) / 2, (Image.Height - Image.Bitmap.Height / Scale) / 2);
                Rectangle.Size.Size := ScanRectangle.Size;
                Rectangle.Visible := True;
              end
              else
                Rectangle.Visible := False;

             if (ACamera.PreviewRotation = ro0) or (ACamera.PreviewRotation = ro180) then
                ACamera.UpdateBitmap(ScanBitmap, @RgbaData[0], RgbaDataWidth, RgbaDataHeight)
              else
                ACamera.UpdateBitmap(ScanBitmap, @RgbaData[0], RgbaDataHeight, RgbaDataWidth);

              Scanning := True;
            end;
          finally
            Dec(Processing);
          end;
        except
          on E: Exception do
            LabelBarcode.Text := E.Message;
        end;
      end);
  end;
end;

procedure TFormMain.ButtonSettingsClick(Sender: TObject);
begin
  Stop;
  FormSettings.Show;
  ButtonStartStop.Enabled := True;
end;

var
  ThreadTerminated: Boolean;

procedure TFormMain.Start;
begin
  if ACamera.PreviewFormat <> pfNV21 then
  begin
    ACamera.PreviewFormat := pfNV21;
    if ACamera.PreviewFormat <> pfNV21 then
      raise Exception.Create('Preview format has to be NV21');
  end;

  RgbaDataWidth := ACamera.PreviewSize.Width;
  RgbaDataHeight := ACamera.PreviewSize.Height;
  SetLength(RgbaData, RgbaDataWidth * RgbaDataHeight);

  LoadLibrary; // show trial message in UI thread

  ACamera.Start;
  ButtonStartStop.Text := 'Stop';

  ThreadTerminated := False;
  TThread.CreateAnonymousThread(
    procedure
    var
      Obr: TFObr;
      Barcode: TObrSymbol;
      Point: TPointF;
      I: Integer;
    begin
      Obr := TFObr.Create(nil);
      Obr.Active := True;
      while not ThreadTerminated do
      try
        if not Scanning then
          Sleep(100)
        else
        try
          ScanBarcode := '';
          ScanRectangle := TRectF.Empty;
          Obr.Picture := ScanBitmap;
          Obr.Scan;
          if Obr.BarcodeCount > 0 then
          begin
            Barcode := Obr.Barcode[0];
            ScanBarcode := Barcode.SymbologyName + Barcode.SymbologyAddonName
              + ' ' + Barcode.OrientationName + ' ' + Barcode.DataUtf8;

            if Barcode.LocationCount >= 2 then
            begin
              ScanRectangle.Left := Barcode.LocationX[0] / Scale;
              ScanRectangle.Top := Barcode.LocationY[0] / Scale;

              for I := 1 to Barcode.LocationCount - 1 do
              begin
                Point.X :=  Barcode.LocationX[I] / Scale;
                Point.Y :=  Barcode.LocationY[I] / Scale;

                if Point.X < ScanRectangle.Left then
                  ScanRectangle.Left := Point.X;
                if Point.X > ScanRectangle.Right then
                  ScanRectangle.Right := Point.X;
                if Point.Y < ScanRectangle.Top then
                  ScanRectangle.Top := Point.Y;
                if Point.Y > ScanRectangle.Bottom then
                  ScanRectangle.Bottom := Point.Y;
              end;

              if ScanRectangle.Height < Scale then
                ScanRectangle.Height := Scale;
              if ScanRectangle.Width < Scale then
                ScanRectangle.Width := Scale;
            end
          end;
        finally
          Scanning := False;
        end;
      except
      end;
    end).Start;
end;

procedure TFormMain.Stop;
begin
  ACamera.Stop;
  ThreadTerminated := True;
  ButtonStartStop.Text := 'Start';
end;

procedure TFormMain.ButtonStartStopClick(Sender: TObject);
begin
  if ACamera.Capturing then
    Stop
  else
    Start
end;

procedure TFormMain.FormCreate(Sender: TObject);
var ScreenSrv: IFMXScreenService;
begin
  ScanBitmap := TBitmap.Create;

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenSrv) then
    Scale := ScreenSrv.GetScreenScale
  else
    Scale := 1.0;

  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, FMXApplicationEventService) then
    FMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent);
end;

function TFormMain.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.EnteredBackground:
    begin
      if ACamera.Capturing then
        Paused := True;
      Stop;
      ACamera.Active := False;
    end;

    TApplicationEvent.WillBecomeForeground:
    begin
      ACamera.Active := True;
      Settings.FormSettings.ApplySettings;
      if Paused then
      begin
        Start;
        Paused := False;
      end;
    end;
  end;

  Result := True;
end;

end.
