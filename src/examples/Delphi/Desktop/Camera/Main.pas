unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FCamera, FMX.ListBox, FMX.Objects,
  FMX.Layouts, FMX.ExtCtrls, Winsoft.FireMonkey.Obr;

type
  TFormMain = class(TForm)
    PanelTop: TPanel;
    PanelRight: TPanel;
    FCamera: TFCamera;
    LabelCamera: TLabel;
    ComboBoxCamera: TComboBox;
    Timer: TTimer;
    Image: TImage;
    LabelFormat: TLabel;
    ComboBoxFormat: TComboBox;
    LabelWrapMode: TLabel;
    ComboBoxWrapMode: TComboBox;
    ButtonSaveCurrentImage: TButton;
    LabelBacklightCompensation: TLabel;
    CheckBoxBacklightCompensation: TCheckBox;
    TrackBarBacklightCompensation: TTrackBar;
    LabelBrightness: TLabel;
    TrackBarBrightness: TTrackBar;
    CheckBoxBrightness: TCheckBox;
    LabelColorEnable: TLabel;
    TrackBarColorEnable: TTrackBar;
    CheckBoxColorEnable: TCheckBox;
    LabelContrast: TLabel;
    TrackBarContrast: TTrackBar;
    CheckBoxContrast: TCheckBox;
    LabelExposure: TLabel;
    TrackBarExposure: TTrackBar;
    CheckBoxExposure: TCheckBox;
    LabelFocus: TLabel;
    TrackBarFocus: TTrackBar;
    CheckBoxFocus: TCheckBox;
    LabelGain: TLabel;
    TrackBarGain: TTrackBar;
    CheckBoxGain: TCheckBox;
    LabelGamma: TLabel;
    TrackBarGamma: TTrackBar;
    CheckBoxGamma: TCheckBox;
    LabelHue: TLabel;
    TrackBarHue: TTrackBar;
    CheckBoxHue: TCheckBox;
    LabelIris: TLabel;
    TrackBarIris: TTrackBar;
    CheckBoxIris: TCheckBox;
    LabelPan: TLabel;
    CheckBoxPan: TCheckBox;
    TrackBarPan: TTrackBar;
    LabelRoll: TLabel;
    TrackBarRoll: TTrackBar;
    CheckBoxRoll: TCheckBox;
    LabelSaturation: TLabel;
    TrackBarSaturation: TTrackBar;
    CheckBoxSaturation: TCheckBox;
    LabelSharpness: TLabel;
    TrackBarSharpness: TTrackBar;
    CheckBoxSharpness: TCheckBox;
    LabelTilt: TLabel;
    TrackBarTilt: TTrackBar;
    CheckBoxTilt: TCheckBox;
    LabelWhiteBalance: TLabel;
    TrackBarWhiteBalance: TTrackBar;
    CheckBoxWhiteBalance: TCheckBox;
    LabelZoom: TLabel;
    TrackBarZoom: TTrackBar;
    CheckBoxZoom: TCheckBox;
    ButtonDefaultValues: TButton;
    FObr: TFObr;
    LabelBarcode: TLabel;
    PanelImage: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxCameraClick(Sender: TObject);
    procedure ComboBoxWrapModeChange(Sender: TObject);
    procedure ButtonSaveCurrentImageClick(Sender: TObject);
    procedure ComboBoxFormatChange(Sender: TObject);
    procedure ComboBoxCameraChange(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure ButtonDefaultValuesClick(Sender: TObject);
    procedure FCameraImageAvailable(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure SetDevices;
    procedure SetFormats;
    procedure SetTrackBars;
    procedure StartDevice;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.SetDevices;
var
  CurrentDeviceName: string;
  Devices: TFVideoCaptureDevices;
  I, Index: Integer;
begin
  ComboBoxCamera.Items.BeginUpdate;
  try
    if ComboBoxCamera.Selected <> nil then
      CurrentDeviceName := ComboBoxCamera.Selected.Text;

    Devices := FCamera.Devices;
    ComboBoxCamera.Items.Clear;
    for I := 0 to Length(Devices) - 1 do
      ComboBoxCamera.Items.Add(Devices[I].Name);

    Index := ComboBoxCamera.Items.IndexOf(CurrentDeviceName);
    if Index <> -1 then
      ComboBoxCamera.ItemIndex := Index
  finally
    ComboBoxCamera.Items.EndUpdate;
  end;
end;

procedure TFormMain.SetFormats;
var
  Formats: TFCameraFormats;
  I: Integer;
begin
  ComboBoxFormat.Items.BeginUpdate;
  try
    ButtonSaveCurrentImage.Enabled := False;
    Timer.Enabled := False;
    FCamera.Active := False;
    FObr.Active := False;
    if ComboBoxCamera.Selected <> nil then
      FCamera.DeviceName := ComboBoxCamera.Selected.Text
    else
      FCamera.DeviceName := '';
    ComboBoxFormat.Items.Clear;
    ComboBoxFormat.Enabled := FCamera.DeviceName <> '';
    if ComboBoxFormat.Enabled then
    begin
      Formats := FCamera.SupportedFormats;
      for I := 0 to Length(Formats) - 1 do
        ComboBoxFormat.Items.Add(IntToStr(Formats[I].Width) + ' x ' + IntToStr(Formats[I].Height) + ' ' + IntToStr(10000000 div Formats[I].AvgTimePerFrame) + 'Hz');
    end
  finally
    ComboBoxFormat.Items.EndUpdate;
  end;
end;

procedure InitTrackBar(TrackbarLabel: TLabel; TrackBar: TTrackBar; CheckBoxAuto: TCheckBox);
begin
  TrackbarLabel.Enabled := False;
  TrackBar.Enabled := False;
  CheckBoxAuto.Enabled := False;
end;

procedure SetTrackBar(TrackbarLabel: TLabel; TrackBar: TTrackBar; CheckBoxAuto: TCheckBox; Position: Integer; Range: TFRange; Auto: Boolean);
begin
  CheckBoxAuto.IsChecked := Auto;
  TrackBar.Min := Range.Min;
  TrackBar.Max := Range.Max;
  TrackBar.Frequency := Range.Delta;

  TrackBar.Value := Position;

  TrackbarLabel.Enabled := True;
  CheckBoxAuto.Enabled := Range.Auto and Range.Manual;
  TrackBar.Enabled := not CheckBoxAuto.IsChecked;
end;

var TrackBarsSetting: Boolean;

procedure TFormMain.SetTrackBars;
begin
  if not TrackBarsSetting then
  try
    TrackBarsSetting := True;
    InitTrackBar(LabelBacklightCompensation, TrackBarBacklightCompensation, CheckBoxBacklightCompensation);
    InitTrackBar(LabelBrightness, TrackBarBrightness, CheckBoxBrightness);
    InitTrackBar(LabelColorEnable, TrackBarColorEnable, CheckBoxColorEnable);
    InitTrackBar(LabelContrast, TrackBarContrast, CheckBoxContrast);
    InitTrackBar(LabelExposure, TrackBarExposure, CheckBoxExposure);
    InitTrackBar(LabelFocus, TrackBarFocus, CheckBoxFocus);
    InitTrackBar(LabelGain, TrackBarGain, CheckBoxGain);
    InitTrackBar(LabelGamma, TrackBarGamma, CheckBoxGamma);
    InitTrackBar(LabelHue, TrackBarHue, CheckBoxHue);
    InitTrackBar(LabelIris, TrackBarIris, CheckBoxIris);
    InitTrackBar(LabelPan, TrackBarPan, CheckBoxPan);
    InitTrackBar(LabelRoll, TrackBarRoll, CheckBoxRoll);
    InitTrackBar(LabelSaturation, TrackBarSaturation, CheckBoxSaturation);
    InitTrackBar(LabelSharpness, TrackBarSharpness, CheckBoxSharpness);
    InitTrackBar(LabelTilt, TrackBarTilt, CheckBoxTilt);
    InitTrackBar(LabelWhiteBalance, TrackBarWhiteBalance, CheckBoxWhiteBalance);
    InitTrackBar(LabelZoom, TrackBarZoom, CheckBoxZoom);

    if FCamera.DeviceName <> '' then
    begin
      try
        SetTrackBar(LabelBacklightCompensation, TrackBarBacklightCompensation, CheckBoxBacklightCompensation, FCamera.BacklightCompensation, FCamera.BacklightCompensationRange, FCamera.BacklightCompensationAuto);
      except
      end;

      try
        SetTrackBar(LabelBrightness, TrackBarBrightness, CheckBoxBrightness, FCamera.Brightness, FCamera.BrightnessRange, FCamera.BrightnessAuto);
      except
      end;

      try
        SetTrackBar(LabelColorEnable, TrackBarColorEnable, CheckBoxColorEnable, FCamera.ColorEnable, FCamera.ColorEnableRange, FCamera.ColorEnableAuto);
      except
      end;

      try
        SetTrackBar(LabelContrast, TrackBarContrast, CheckBoxContrast, FCamera.Contrast, FCamera.ContrastRange, FCamera.ContrastAuto);
      except
      end;

      try
        SetTrackBar(LabelExposure, TrackBarExposure, CheckBoxExposure, FCamera.Exposure, FCamera.ExposureRange, FCamera.ExposureAuto);
      except
      end;

      try
        SetTrackBar(LabelFocus, TrackBarFocus, CheckBoxFocus, FCamera.Focus, FCamera.FocusRange, FCamera.FocusAuto);
      except
      end;

      try
        SetTrackBar(LabelGain, TrackBarGain, CheckBoxGain, FCamera.Gain, FCamera.GainRange, FCamera.GainAuto);
      except
      end;

      try
        SetTrackBar(LabelGamma, TrackBarGamma, CheckBoxGamma, FCamera.Gamma, FCamera.GammaRange, FCamera.GammaAuto);
      except
      end;

      try
        SetTrackBar(LabelHue, TrackBarHue, CheckBoxHue, FCamera.Hue, FCamera.HueRange, FCamera.HueAuto);
      except
      end;

      try
        SetTrackBar(LabelIris, TrackBarIris, CheckBoxIris, FCamera.Iris, FCamera.IrisRange, FCamera.IrisAuto);
      except
      end;

      try
        SetTrackBar(LabelPan, TrackBarPan, CheckBoxPan, FCamera.Pan, FCamera.PanRange, FCamera.PanAuto);
      except
      end;

      try
        SetTrackBar(LabelRoll, TrackBarRoll, CheckBoxRoll, FCamera.Roll, FCamera.RollRange, FCamera.RollAuto);
      except
      end;

      try
        SetTrackBar(LabelSaturation, TrackBarSaturation, CheckBoxSaturation, FCamera.Saturation, FCamera.SaturationRange, FCamera.SaturationAuto);
      except
      end;

      try
        SetTrackBar(LabelSharpness, TrackBarSharpness, CheckBoxSharpness, FCamera.Sharpness, FCamera.SharpnessRange, FCamera.SharpnessAuto);
      except
      end;

      try
        SetTrackBar(LabelTilt, TrackBarTilt, CheckBoxTilt, FCamera.Tilt, FCamera.TiltRange, FCamera.TiltAuto);
      except
      end;

      try
        SetTrackBar(LabelWhiteBalance, TrackBarWhiteBalance, CheckBoxWhiteBalance, FCamera.WhiteBalance, FCamera.WhiteBalanceRange, FCamera.WhiteBalanceAuto);
      except
      end;

      try
        SetTrackBar(LabelZoom, TrackBarZoom, CheckBoxZoom, FCamera.Zoom, FCamera.ZoomRange, FCamera.ZoomAuto);
      except
      end;
    end
  finally
    TrackBarsSetting := False;
  end;
end;

procedure TFormMain.StartDevice;
begin
  SetTrackBars;
  if ComboBoxFormat.ItemIndex <> -1 then
  begin
    Timer.Enabled := False;
    FCamera.Active := False;
    FObr.Active := False;
    FCamera.Format := FCamera.SupportedFormats[ComboBoxFormat.ItemIndex];
    FObr.Active := True;
    FCamera.Active := True;
    FCamera.Run;
    if FCamera.CaptureType = ctVmr9 then
      Timer.Enabled := True;
    ButtonSaveCurrentImage.Enabled := True;
  end
end;

procedure TFormMain.TrackBarChange(Sender: TObject);
begin
  if not TrackBarsSetting then
    if Sender = TrackBarBacklightCompensation then
      FCamera.BacklightCompensation := Trunc(TrackBarBacklightCompensation.Value)
    else if Sender = TrackBarBrightness then
      FCamera.Brightness := Trunc(TrackBarBrightness.Value)
    else if Sender = TrackBarColorEnable then
      FCamera.ColorEnable := Trunc(TrackBarColorEnable.Value)
    else if Sender = TrackBarContrast then
      FCamera.Contrast := Trunc(TrackBarContrast.Value)
    else if Sender = TrackBarExposure then
      FCamera.Exposure := Trunc(TrackBarExposure.Value)
    else if Sender = TrackBarFocus then
      FCamera.Focus := Trunc(TrackBarFocus.Value)
    else if Sender = TrackBarGain then
      FCamera.Gain := Trunc(TrackBarGain.Value)
    else if Sender = TrackBarGamma then
      FCamera.Gamma := Trunc(TrackBarGamma.Value)
    else if Sender = TrackBarHue then
      FCamera.Hue := Trunc(TrackBarHue.Value)
    else if Sender = TrackBarIris then
      FCamera.Iris := Trunc(TrackBarIris.Value)
    else if Sender = TrackBarPan then
      FCamera.Pan := Trunc(TrackBarPan.Value)
    else if Sender = TrackBarRoll then
      FCamera.Roll := Trunc(TrackBarRoll.Value)
    else if Sender = TrackBarSaturation then
      FCamera.Saturation := Trunc(TrackBarSaturation.Value)
    else if Sender = TrackBarSharpness then
      FCamera.Sharpness := Trunc(TrackBarSharpness.Value)
    else if Sender = TrackBarTilt then
      FCamera.Tilt := Trunc(TrackBarTilt.Value)
    else if Sender = TrackBarWhiteBalance then
      FCamera.WhiteBalance := Trunc(TrackBarWhiteBalance.Value)
    else if Sender = TrackBarZoom then
      FCamera.Zoom := Trunc(TrackBarZoom.Value);
end;

procedure TFormMain.ButtonDefaultValuesClick(Sender: TObject);
begin
  try
    try
      FCamera.BacklightCompensation := FCamera.BacklightCompensationRange.Default;
    except
    end;

    try
      FCamera.Brightness := FCamera.BrightnessRange.Default;
    except
    end;

    try
      FCamera.ColorEnable := FCamera.ColorEnableRange.Default;
    except
    end;

    try
      FCamera.Contrast := FCamera.ContrastRange.Default;
    except
    end;

    try
      FCamera.Exposure := FCamera.ExposureRange.Default;
    except
    end;

    try
      FCamera.Focus := FCamera.FocusRange.Default;
    except
    end;

    try
      FCamera.Gain := FCamera.GainRange.Default;
    except
    end;

    try
      FCamera.Gamma := FCamera.GammaRange.Default;
    except
    end;

    try
      FCamera.Hue := FCamera.HueRange.Default;
    except
    end;

    try
      FCamera.Iris := FCamera.IrisRange.Default;
    except
    end;

    try
      FCamera.Pan := FCamera.PanRange.Default;
    except
    end;

    try
      FCamera.Roll := FCamera.RollRange.Default;
    except
    end;

    try
      FCamera.Saturation := FCamera.SaturationRange.Default;
    except
    end;

    try
      FCamera.Sharpness := FCamera.SharpnessRange.Default;
    except
    end;

    try
      FCamera.Tilt := FCamera.TiltRange.Default;
    except
    end;

    try
      FCamera.WhiteBalance := FCamera.WhiteBalanceRange.Default;
    except
    end;

    try
      FCamera.Zoom := FCamera.ZoomRange.Default;
    except
    end;
  finally
    SetTrackBars;
  end
end;

procedure TFormMain.ButtonSaveCurrentImageClick(Sender: TObject);
begin
  FCamera.CurrentImageToFile('image.bmp');
end;

procedure TFormMain.CheckBoxChange(Sender: TObject);
begin
  if not TrackBarsSetting then
  begin
    if Sender = CheckBoxBacklightCompensation then
      FCamera.BacklightCompensationAuto := CheckBoxBacklightCompensation.IsChecked
    else if Sender = CheckBoxBrightness then
      FCamera.BrightnessAuto := CheckBoxBrightness.IsChecked
    else if Sender = CheckBoxColorEnable then
      FCamera.ColorEnableAuto := CheckBoxColorEnable.IsChecked
    else if Sender = CheckBoxContrast then
      FCamera.ContrastAuto := CheckBoxContrast.IsChecked
    else if Sender = CheckBoxExposure then
      FCamera.ExposureAuto := CheckBoxExposure.IsChecked
    else if Sender = CheckBoxFocus then
      FCamera.FocusAuto := CheckBoxFocus.IsChecked
    else if Sender = CheckBoxGain then
      FCamera.GainAuto := CheckBoxGain.IsChecked
    else if Sender = CheckBoxGamma then
      FCamera.GammaAuto := CheckBoxGamma.IsChecked
    else if Sender = CheckBoxHue then
      FCamera.HueAuto := CheckBoxHue.IsChecked
    else if Sender = CheckBoxIris then
      FCamera.IrisAuto := CheckBoxIris.IsChecked
    else if Sender = CheckBoxPan then
      FCamera.PanAuto := CheckBoxPan.IsChecked
    else if Sender = CheckBoxRoll then
      FCamera.RollAuto := CheckBoxRoll.IsChecked
    else if Sender = CheckBoxSaturation then
      FCamera.SaturationAuto := CheckBoxSaturation.IsChecked
    else if Sender = CheckBoxSharpness then
      FCamera.SharpnessAuto := CheckBoxSharpness.IsChecked
    else if Sender = CheckBoxTilt then
      FCamera.TiltAuto := CheckBoxTilt.IsChecked
    else if Sender = CheckBoxWhiteBalance then
      FCamera.WhiteBalanceAuto := CheckBoxWhiteBalance.IsChecked
    else if Sender = CheckBoxZoom then
      FCamera.ZoomAuto := CheckBoxZoom.IsChecked;

    SetTrackBars;
  end
end;

procedure TFormMain.ComboBoxCameraChange(Sender: TObject);
begin
  SetFormats;
  SetTrackBars;
  ButtonDefaultValues.Enabled := FCamera.DeviceName <> '';
end;

procedure TFormMain.ComboBoxCameraClick(Sender: TObject);
begin
  SetDevices;
end;

procedure TFormMain.ComboBoxFormatChange(Sender: TObject);
begin
  StartDevice;
end;

procedure TFormMain.ComboBoxWrapModeChange(Sender: TObject);
var WrapMode: TImageWrapMode;
begin
  case ComboBoxWrapMode.ItemIndex of
    0: WrapMode := TImageWrapMode.Center;
    1: WrapMode := TImageWrapMode.Fit;
    2: WrapMode := TImageWrapMode.Original;
    3: WrapMode := TImageWrapMode.Stretch;
    4: WrapMode := TImageWrapMode.Tile;
    else WrapMode := TImageWrapMode.Fit;
  end;
  Image.WrapMode := WrapMode;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  SetDevices;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  FCameraImageAvailable(nil);
end;

procedure TFormMain.FCameraImageAvailable(Sender: TObject);
var
  Bitmap: TBitmap;
  Barcode: TObrSymbol;
begin
  Bitmap := FCamera.CurrentImageToBitmap;
  try
    Image.Bitmap := Bitmap;
    FObr.Picture.Assign(Bitmap);
    FObr.Scan;
    if FObr.BarcodeCount > 0 then
    begin
      Barcode := FObr.Barcode[0];
      LabelBarcode.Text := Barcode.SymbologyName + Barcode.SymbologyAddonName + ' ' + Barcode.OrientationName + ' ' + Barcode.DataUtf8;
    end
  finally
    Bitmap.Free;
  end;
end;

end.
