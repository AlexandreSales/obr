unit Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.Memo, FMX.ScrollBox, FMX.Controls.Presentation,
  Winsoft.Android.Camera;

type
  TFormSettings = class(TForm)
    LabelCamera: TLabel;
    ComboBoxCamera: TComboBox;
    LabelPreviewSize: TLabel;
    ComboBoxPreviewSize: TComboBox;
    LabelProperties: TLabel;
    ButtonOk: TButton;
    MemoProperties: TMemo;
    ComboBoxFocusMode: TComboBox;
    LabelFocusMode: TLabel;
    ComboBoxFlashMode: TComboBox;
    LabelFlashMode: TLabel;
    VertScrollBox: TVertScrollBox;
    ComboBoxColorEffect: TComboBox;
    LabelColorEffect: TLabel;
    ComboBoxAntibanding: TComboBox;
    LabelAntibanding: TLabel;
    ComboBoxWhiteBalance: TComboBox;
    LabelWhiteBalance: TLabel;
    CheckBoxStretchToScreen: TCheckBox;
    procedure ComboBoxCameraChange(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ComboBoxPreviewSizeChange(Sender: TObject);
    procedure ComboBoxFocusModeChange(Sender: TObject);
    procedure ComboBoxFlashModeChange(Sender: TObject);
    procedure ComboBoxColorEffectChange(Sender: TObject);
    procedure ComboBoxAntibandingChange(Sender: TObject);
    procedure ComboBoxWhiteBalanceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxStretchToScreenChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplySettings;
  end;

var
  FormSettings: TFormSettings;

implementation

uses Main, Rtti, FMX.Objects;

{$R *.fmx}

function BooleanToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False'
end;

procedure TFormSettings.ButtonOkClick(Sender: TObject);
begin
  FormMain.Show;
  FormMain.Start;
end;

procedure TFormSettings.ComboBoxCameraChange(Sender: TObject);
var
  I, PreviewIndex: Integer;
  Camera: TACamera;
  CameraInfo: TCameraInfo;
  Lines: TStrings;
  SupportedPreviewSizes: TArray<TPreviewSize>;
  FlashMode: TFlashMode;
  FocusMode: TFocusMode;
  Antibanding: TAntibanding;
  ColorEffect: TColorEffect;
  WhiteBalance: TWhiteBalance;
  PreviewFormat: TPreviewFormat;
begin
  Camera := FormMain.ACamera;
  Camera.Active := False;

  ComboBoxPreviewSize.ItemIndex := -1;
  ComboBoxPreviewSize.Items.Clear;

  ComboBoxFocusMode.ItemIndex := -1;
  ComboBoxFocusMode.Items.Clear;
  ComboBoxFocusMode.Enabled := False;

  ComboBoxFlashMode.ItemIndex := -1;
  ComboBoxFlashMode.Items.Clear;
  ComboBoxFlashMode.Enabled := False;

  ComboBoxColorEffect.ItemIndex := -1;
  ComboBoxColorEffect.Items.Clear;
  ComboBoxColorEffect.Enabled := False;

  ComboBoxAntibanding.ItemIndex := -1;
  ComboBoxAntibanding.Items.Clear;
  ComboBoxAntibanding.Enabled := False;

  ComboBoxWhiteBalance.ItemIndex := -1;
  ComboBoxWhiteBalance.Items.Clear;
  ComboBoxWhiteBalance.Enabled := False;

  Camera.CameraIndex := ComboBoxCamera.ItemIndex;
  Camera.Active := True;

  // set preview sizes
  PreviewIndex := 0;
  SupportedPreviewSizes := Camera.SupportedPreviewSizes;
  for I := 0 to Length(SupportedPreviewSizes) - 1 do
  begin
    ComboBoxPreviewSize.Items.Insert(0, IntToStr(SupportedPreviewSizes[I].Width) + ' x ' + IntToStr(SupportedPreviewSizes[I].Height));
    if (Camera.PreferredPreviewSize.Width = SupportedPreviewSizes[I].Width)
     and (Camera.PreferredPreviewSize.Height = SupportedPreviewSizes[I].Height) then
       PreviewIndex := Length(SupportedPreviewSizes) - 1 - I;
  end;

  ComboBoxPreviewSize.ItemIndex := PreviewIndex;
  ComboBoxPreviewSizeChange(Sender);

  // set focus modes
  for FocusMode in Camera.SupportedFocusModes do
    if FocusMode <> foUnknown then
    begin
      ComboBoxFocusMode.Items.Add(TRttiEnumerationType.GetName(FocusMode));
      if FocusMode = foContinuousVideo then
        ComboBoxFocusMode.ItemIndex := ComboBoxFocusMode.Items.Count - 1;
    end;
  if ComboBoxFocusMode.Items.Count > 0 then
  begin
    ComboBoxFocusMode.Enabled := True;
    if ComboBoxFocusMode.ItemIndex = -1  then
      ComboBoxFocusMode.ItemIndex := 0;
    ComboBoxFocusModeChange(Sender);
  end;

  // set flash modes
  for FlashMode in Camera.SupportedFlashModes do
    if FlashMode <> flUnknown then
    begin
      ComboBoxFlashMode.Items.Add(TRttiEnumerationType.GetName(FlashMode));
      if FlashMode = Camera.FlashMode then
        ComboBoxFlashMode.ItemIndex := ComboBoxFlashMode.Items.Count - 1;
    end;
  if ComboBoxFlashMode.Items.Count > 0 then
  begin
    ComboBoxFlashMode.Enabled := True;
    if ComboBoxFlashMode.ItemIndex = -1  then
      ComboBoxFlashMode.ItemIndex := 0;
    ComboBoxFlashModeChange(Sender);
  end;

  // set color effects
  for ColorEffect in Camera.SupportedColorEffects do
    if ColorEffect <> ceUnknown then
    begin
      ComboBoxColorEffect.Items.Add(TRttiEnumerationType.GetName(ColorEffect));
      if ColorEffect = Camera.ColorEffect then
        ComboBoxColorEffect.ItemIndex := ComboBoxColorEffect.Items.Count - 1;
    end;
  if ComboBoxColorEffect.Items.Count > 0 then
  begin
    ComboBoxColorEffect.Enabled := True;
    if ComboBoxColorEffect.ItemIndex = -1  then
      ComboBoxColorEffect.ItemIndex := 0;
    ComboBoxColorEffectChange(Sender);
  end;

  // set antibanding
  for Antibanding in Camera.SupportedAntibandings do
    if Antibanding <> abUnknown then
    begin
      ComboBoxAntibanding.Items.Add(TRttiEnumerationType.GetName(Antibanding));
      if Antibanding = Camera.Antibanding then
        ComboBoxAntibanding.ItemIndex := ComboBoxAntibanding.Items.Count - 1;
    end;
  if ComboBoxAntibanding.Items.Count > 0 then
  begin
    ComboBoxAntibanding.Enabled := True;
    if ComboBoxAntibanding.ItemIndex = -1  then
      ComboBoxAntibanding.ItemIndex := 0;
    ComboBoxAntibandingChange(Sender);
  end;

  // set white balance
  for WhiteBalance in Camera.SupportedWhiteBalance do
    if WhiteBalance <> wbUnknown then
    begin
      ComboBoxWhiteBalance.Items.Add(TRttiEnumerationType.GetName(WhiteBalance));
      if WhiteBalance = Camera.WhiteBalance then
        ComboBoxWhiteBalance.ItemIndex := ComboBoxWhiteBalance.Items.Count - 1;
    end;
  if ComboBoxWhiteBalance.Items.Count > 0 then
  begin
    ComboBoxWhiteBalance.Enabled := True;
    if ComboBoxWhiteBalance.ItemIndex = -1  then
      ComboBoxWhiteBalance.ItemIndex := 0;
    ComboBoxWhiteBalanceChange(Sender);
  end;

  Lines := MemoProperties.Lines;
  Lines.Clear;

  CameraInfo := Camera.CameraInfo[ComboBoxCamera.ItemIndex];
  Lines.Add('Orientation: ' + IntToStr(CameraInfo.Orientation));
  Lines.Add('Can disable shutter sound: ' + BooleanToStr(CameraInfo.CanDisableShutterSound));

  Lines.Add('Zoom suppported: ' + BooleanToStr(Camera.ZoomSupported));
  if Camera.ZoomSupported then
  begin
    Lines.Add('Zoom: ' + IntToStr(Camera.Zoom));
    Lines.Add('Max zoom: ' + IntToStr(Camera.MaxZoom));
  end;
  Lines.Add('Smooth zoom supported: ' + BooleanToStr(Camera.SmoothZoomSupported));

  Lines.Add('Video stabilization suppported: ' + BooleanToStr(Camera.VideoStabilizationSupported));
  if Camera.VideoStabilizationSupported then
    Lines.Add('Video stabilization: ' + BooleanToStr(Camera.VideoStabilization));

  Lines.Add('Focal length: ' + FloatToStrF(Camera.FocalLength, ffFixed, 10, 2));
  Lines.Add('Vertical view angle: ' + FloatToStrF(Camera.VerticalViewAngle, ffFixed, 10, 2));
  Lines.Add('Min exposure compensation: ' + IntToStr(Camera.MinExposureCompensation));
  Lines.Add('Max exposure compensation: ' + IntToStr(Camera.MaxExposureCompensation));
  Lines.Add('Exposure compensation: ' + IntToStr(Camera.ExposureCompensation));
  Lines.Add('Exposure compensation step: ' + FloatToStrF(Camera.ExposureCompensationStep, ffFixed, 10, 2));
  Lines.Add('EV: ' + FloatToStrF(Camera.EV, ffFixed, 10, 2));

  for PreviewFormat in Camera.SupportedPreviewFormats do
    Lines.Add('Supported preview format: ' + TRttiEnumerationType.GetName(PreviewFormat));
  if Camera.SupportedPreviewFormats <> [] then
    Lines.Add('Preview format: ' + TRttiEnumerationType.GetName(Camera.PreviewFormat));

  Lines.Add('Preferred preview size: ' + IntToStr(Camera.PreferredPreviewSize.Width) + ' x ' + IntToStr(Camera.PreferredPreviewSize.Height));
  Lines.Add('Preview FPS: ' + FloatToStr(Camera.PreviewFps.Min / 1000.0) + ' - ' + FloatToStr(Camera.PreviewFps.Max / 1000.0));
end;

procedure TFormSettings.ComboBoxPreviewSizeChange(Sender: TObject);
var SupportedPreviewSizes: TArray<TPreviewSize>;
begin
  if ComboBoxPreviewSize.ItemIndex = -1 then
    Exit;

  SupportedPreviewSizes := FormMain.ACamera.SupportedPreviewSizes;
  FormMain.ACamera.PreviewSize := SupportedPreviewSizes[Length(SupportedPreviewSizes) - ComboBoxPreviewSize.ItemIndex - 1];
end;

procedure TFormSettings.ComboBoxFocusModeChange(Sender: TObject);
var FocusMode: TFocusMode;
begin
  if ComboBoxFocusMode.ItemIndex = -1 then
    Exit;

  for FocusMode := Low(TFocusMode) to High(TFocusMode) do
    if TRttiEnumerationType.GetName(FocusMode) = ComboBoxFocusMode.Items[ComboBoxFocusMode.ItemIndex] then
    begin
      FormMain.ACamera.FocusMode := FocusMode;
      Break;
    end;
end;

procedure TFormSettings.ComboBoxFlashModeChange(Sender: TObject);
var FlashMode: TFlashMode;
begin
  if ComboBoxFlashMode.ItemIndex = -1 then
    Exit;

  for FlashMode := Low(TFlashMode) to High(TFlashMode) do
    if TRttiEnumerationType.GetName(FlashMode) = ComboBoxFlashMode.Items[ComboBoxFlashMode.ItemIndex] then
    begin
      FormMain.ACamera.FlashMode := FlashMode;
      Break;
    end;
end;

procedure TFormSettings.ComboBoxColorEffectChange(Sender: TObject);
var ColorEffect: TColorEffect;
begin
  if ComboBoxColorEffect.ItemIndex = -1 then
    Exit;

  for ColorEffect := Low(TColorEffect) to High(TColorEffect) do
    if TRttiEnumerationType.GetName(ColorEffect) = ComboBoxColorEffect.Items[ComboBoxColorEffect.ItemIndex] then
    begin
      FormMain.ACamera.ColorEffect := ColorEffect;
      Break;
    end;
end;

procedure TFormSettings.CheckBoxStretchToScreenChange(Sender: TObject);
begin
  if CheckBoxStretchToScreen.IsChecked then
    Main.FormMain.Image.WrapMode := TImageWrapMode.Stretch
  else
    Main.FormMain.Image.WrapMode := TImageWrapMode.Center
end;

procedure TFormSettings.ComboBoxAntibandingChange(Sender: TObject);
var Antibanding: TAntibanding;
begin
  if ComboBoxAntibanding.ItemIndex = -1 then
    Exit;

  for Antibanding := Low(TAntibanding) to High(TAntibanding) do
    if TRttiEnumerationType.GetName(Antibanding) = ComboBoxAntibanding.Items[ComboBoxAntibanding.ItemIndex] then
    begin
      FormMain.ACamera.Antibanding := Antibanding;
      Break;
    end;
end;

procedure TFormSettings.ComboBoxWhiteBalanceChange(Sender: TObject);
var WhiteBalance: TWhiteBalance;
begin
  if ComboBoxWhiteBalance.ItemIndex = -1 then
    Exit;

  for WhiteBalance := Low(TWhiteBalance) to High(TWhiteBalance) do
    if TRttiEnumerationType.GetName(WhiteBalance) = ComboBoxWhiteBalance.Items[ComboBoxWhiteBalance.ItemIndex] then
    begin
      FormMain.ACamera.WhiteBalance := WhiteBalance;
      Break;
    end;
end;

procedure TFormSettings.ApplySettings;
begin
  ComboBoxPreviewSizeChange(nil);
  ComboBoxFocusModeChange(nil);
  ComboBoxFlashModeChange(nil);
  ComboBoxColorEffectChange(nil);
  ComboBoxAntibandingChange(nil);
  ComboBoxWhiteBalanceChange(nil);
end;

procedure TFormSettings.FormCreate(Sender: TObject);
var
  I: Integer;
  CameraText: string;
begin
  for I := 0 to FormMain.ACamera.CameraCount - 1 do
  begin
    CameraText := 'Camera ' + IntToStr(I);

    if FormMain.ACamera.CameraInfo[I].Facing = cfBack then
      CameraText := CameraText + ' (Back)'
    else
      CameraText := CameraText + ' (Front)';

    ComboBoxCamera.Items.Add(CameraText);
  end;

  ComboBoxCamera.ItemIndex := 0;
end;

end.
