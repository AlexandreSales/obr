//---------------------------------------------------------------------------
#include <fmx.h>
#pragma hdrstop

#include "Settings.h"
#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TFormSettings *FormSettings;

TFocusMode FocusModes[] = { foAuto, foContinuousPicture, foContinuousVideo, foEdof, foFixed, foInfinity, foMacro };
String FocusModeNames[] = { "foAuto", "foContinuousPicture", "foContinuousVideo", "foEdof", "foFixed", "foInfinity", "foMacro" };

TFlashMode FlashModes[] = { flOff, flAuto, flOn, flTorch, flRedEye };
String FlashModeNames[] = { "flOff", "flAuto", "flOn", "flTorch", "flRedEye" };

TColorEffect ColorEffects[] = { ceNone, ceMono, ceNegative, ceSolarize, ceSepia, cePosterize, ceWhiteBoard, ceBlackBoard, ceAqua, ceEmboss, ceSketch, ceNeon };
String ColorEffectNames[] = { "ceNone", "ceMono", "ceNegative", "ceSolarize", "ceSepia", "cePosterize", "ceWhiteBoard", "ceBlackBoard", "ceAqua", "ceEmboss", "ceSketch", "ceNeon" };

TAntibanding Antibandings[] = { abOff, abAuto, ab50Hz, ab60Hz };
String AntibandingNames[] = { "abOff", "abAuto", "ab50Hz", "ab60Hz" };

TWhiteBalance WhiteBalances[] = { wbAuto, wbCloudyDaylight, wbDaylight, wbFluorescent, wbIncandescent, wbShade, wbTwilight, wbWarmFluorescent };
String WhiteBalanceNames[] = { "wbAuto", "wbCloudyDaylight", "wbDaylight", "wbFluorescent", "wbIncandescent", "wbShade", "wbTwilight", "wbWarmFluorescent" };

//---------------------------------------------------------------------------
String BooleanToStr(bool value)
{
  return value ? "True" : "False";
}
//---------------------------------------------------------------------------
__fastcall TFormSettings::TFormSettings(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ButtonOkClick(TObject *Sender)
{
  FormMain->Show();
  FormMain->Start();
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ComboBoxCameraChange(TObject *Sender)
{
  TACamera *camera = FormMain->ACamera;
  camera->Active = false;

  ComboBoxPreviewSize->ItemIndex = -1;
  ComboBoxPreviewSize->Items->Clear();

  ComboBoxFocusMode->ItemIndex = -1;
  ComboBoxFocusMode->Items->Clear();
  ComboBoxFocusMode->Enabled = false;

  ComboBoxFlashMode->ItemIndex = -1;
  ComboBoxFlashMode->Items->Clear();
  ComboBoxFlashMode->Enabled = false;

  ComboBoxColorEffect->ItemIndex = -1;
  ComboBoxColorEffect->Items->Clear();
  ComboBoxColorEffect->Enabled = false;

  ComboBoxAntibanding->ItemIndex = -1;
  ComboBoxAntibanding->Items->Clear();
  ComboBoxAntibanding->Enabled = false;

  ComboBoxWhiteBalance->ItemIndex = -1;
  ComboBoxWhiteBalance->Items->Clear();
  ComboBoxWhiteBalance->Enabled = false;

  camera->CameraIndex = ComboBoxCamera->ItemIndex;
  camera->Active = true;

  // set preview sizes
  System::DynamicArray<TPreviewSize> supportedPreviewSizes = camera->SupportedPreviewSizes;
  for (int i = 0; i < supportedPreviewSizes.Length; ++i)
	ComboBoxPreviewSize->Items->Insert(0, IntToStr(supportedPreviewSizes[i].Width) + " x " + IntToStr(supportedPreviewSizes[i].Height));
  ComboBoxPreviewSize->ItemIndex = 0;
  ComboBoxPreviewSizeChange(Sender);

  // set focus modes
  TFocusModes supportedFocusModes = camera->SupportedFocusModes;
  for (int i = 0; i < sizeof(FocusModes) / sizeof(FocusModes[0]); ++i)
	if (supportedFocusModes.Contains(FocusModes[i]))
	{
	  ComboBoxFocusMode->Items->Add(FocusModeNames[i]);
	  if (FocusModes[i] == foContinuousVideo)
		ComboBoxFocusMode->ItemIndex = ComboBoxFocusMode->Items->Count - 1;
	}
  if (ComboBoxFocusMode->Items->Count > 0)
  {
	ComboBoxFocusMode->Enabled = true;
	if (ComboBoxFocusMode->ItemIndex == -1)
	  ComboBoxFocusMode->ItemIndex = 0;
	ComboBoxFocusModeChange(Sender);
  }

  // set flash modes
  TFlashModes supportedFlashModes = camera->SupportedFlashModes;
  for (int i = 0; i < sizeof(FlashModes) / sizeof(FlashModes[0]); ++i)
	if (supportedFlashModes.Contains(FlashModes[i]))
	{
	  ComboBoxFlashMode->Items->Add(FlashModeNames[i]);
	  if (FlashModes[i] == camera->FlashMode)
		ComboBoxFlashMode->ItemIndex = ComboBoxFlashMode->Items->Count - 1;
	}
  if (ComboBoxFlashMode->Items->Count > 0)
  {
	ComboBoxFlashMode->Enabled = true;
	if (ComboBoxFlashMode->ItemIndex == -1)
	  ComboBoxFlashMode->ItemIndex = 0;
	ComboBoxFlashModeChange(Sender);
  }

  // set color effects
  TColorEffects supportedColorEffects = camera->SupportedColorEffects;
  for (int i = 0; i < sizeof(ColorEffects) / sizeof(ColorEffects[0]); ++i)
	if (supportedColorEffects.Contains(ColorEffects[i]))
	{
	  ComboBoxColorEffect->Items->Add(ColorEffectNames[i]);
	  if (ColorEffects[i] == camera->ColorEffect)
		ComboBoxColorEffect->ItemIndex = ComboBoxColorEffect->Items->Count - 1;
	}
  if (ComboBoxColorEffect->Items->Count > 0)
  {
	ComboBoxColorEffect->Enabled = true;
	if (ComboBoxColorEffect->ItemIndex == -1)
	  ComboBoxColorEffect->ItemIndex = 0;
	ComboBoxColorEffectChange(Sender);
  }

  // set antibanding
  TAntibandings supportedAntibandings = camera->SupportedAntibandings;
  for (int i = 0; i < sizeof(Antibandings) / sizeof(Antibandings[0]); ++i)
	if (supportedAntibandings.Contains(Antibandings[i]))
	{
	  ComboBoxAntibanding->Items->Add(AntibandingNames[i]);
	  if (Antibandings[i] == camera->Antibanding)
		ComboBoxAntibanding->ItemIndex = ComboBoxAntibanding->Items->Count - 1;
	}
  if (ComboBoxAntibanding->Items->Count > 0)
  {
	ComboBoxAntibanding->Enabled = true;
	if (ComboBoxAntibanding->ItemIndex == -1)
	  ComboBoxAntibanding->ItemIndex = 0;
	ComboBoxAntibandingChange(Sender);
  }

  // set white balance
  TWhiteBalances supportedWhiteBalance = camera->SupportedWhiteBalance;
  for (int i = 0; i < sizeof(WhiteBalances) / sizeof(WhiteBalances[0]); ++i)
	if (supportedWhiteBalance.Contains(WhiteBalances[i]))
	{
	  ComboBoxWhiteBalance->Items->Add(WhiteBalanceNames[i]);
	  if (WhiteBalances[i] == camera->WhiteBalance)
		ComboBoxWhiteBalance->ItemIndex = ComboBoxWhiteBalance->Items->Count - 1;
    }
  if (ComboBoxWhiteBalance->Items->Count > 0)
  {
	ComboBoxWhiteBalance->Enabled = true;
	if (ComboBoxWhiteBalance->ItemIndex == -1)
	  ComboBoxWhiteBalance->ItemIndex = 0;
	ComboBoxWhiteBalanceChange(Sender);
  }

  TStrings *lines = MemoProperties->Lines;
  lines->Clear();

  TCameraInfo cameraInfo = camera->CameraInfo[ComboBoxCamera->ItemIndex];
  lines->Add("Orientation: " + IntToStr(cameraInfo.Orientation));
  lines->Add("Can disable shutter sound: " + BooleanToStr(cameraInfo.CanDisableShutterSound));

  lines->Add("Zoom suppported: " + BooleanToStr(camera->ZoomSupported));
  if (camera->ZoomSupported)
  {
	lines->Add("Zoom: " + IntToStr(camera->Zoom));
	lines->Add("Max zoom: " + IntToStr(camera->MaxZoom));
  }
  lines->Add("Smooth zoom supported: " + BooleanToStr(camera->SmoothZoomSupported));

  lines->Add("Video stabilization suppported: " + BooleanToStr(camera->VideoStabilizationSupported));
  if (camera->VideoStabilizationSupported)
	lines->Add("Video stabilization: " + BooleanToStr(camera->VideoStabilization));

  lines->Add("Focal length: " + FloatToStrF(camera->FocalLength, ffFixed, 10, 2));
  lines->Add("Vertical view angle: " + FloatToStrF(camera->VerticalViewAngle, ffFixed, 10, 2));
  lines->Add("Min exposure compensation: " + IntToStr(camera->MinExposureCompensation));
  lines->Add("Max exposure compensation: " + IntToStr(camera->MaxExposureCompensation));
  lines->Add("Exposure compensation: " + IntToStr(camera->ExposureCompensation));
  lines->Add("Exposure compensation step: " + FloatToStrF(camera->ExposureCompensationStep, ffFixed, 10, 2));
  lines->Add("EV: " + FloatToStrF(camera->EV, ffFixed, 10, 2));

  lines->Add("Preferred preview size: " + IntToStr(camera->PreferredPreviewSize.Width) + " x " + IntToStr(camera->PreferredPreviewSize.Height));
  lines->Add("Preview FPS: " + FloatToStr(camera->PreviewFps.Min / 1000.0) + " - " + FloatToStr(camera->PreviewFps.Max / 1000.0));
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ComboBoxPreviewSizeChange(TObject *Sender)
{
  if (ComboBoxPreviewSize->ItemIndex == -1)
	return;

  System::DynamicArray<TPreviewSize> supportedPreviewSizes = FormMain->ACamera->SupportedPreviewSizes;
  FormMain->ACamera->PreviewSize = supportedPreviewSizes[supportedPreviewSizes.Length - ComboBoxPreviewSize->ItemIndex - 1];
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ComboBoxFocusModeChange(TObject *Sender)
{
  if (ComboBoxFocusMode->ItemIndex == -1)
	return;

  String focusModeName = (*ComboBoxFocusMode->Items)[ComboBoxFocusMode->ItemIndex];
  for (int i = 0; i < sizeof(FocusModes) / sizeof(FocusModes[0]); ++i)
	if (focusModeName == FocusModeNames[i])
	{
	  FormMain->ACamera->FocusMode = FocusModes[i];
	  break;
	}
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ComboBoxFlashModeChange(TObject *Sender)
{
  if (ComboBoxFlashMode->ItemIndex == -1)
	return;

  String flashModeName = (*ComboBoxFlashMode->Items)[ComboBoxFlashMode->ItemIndex];
  for (int i = 0; i < sizeof(FlashModes) / sizeof(FlashModes[0]); ++i)
	if (flashModeName == FlashModeNames[i])
	{
	  FormMain->ACamera->FlashMode = FlashModes[i];
	  break;
	}
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ComboBoxColorEffectChange(TObject *Sender)
{
  if (ComboBoxColorEffect->ItemIndex == -1)
	return;

  String colorEffectName = (*ComboBoxColorEffect->Items)[ComboBoxColorEffect->ItemIndex];
  for (int i = 0; i < sizeof(ColorEffects) / sizeof(ColorEffects[0]); ++i)
	if (colorEffectName == ColorEffectNames[i])
	{
	  FormMain->ACamera->ColorEffect = ColorEffects[i];
	  break;
	}
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ComboBoxAntibandingChange(TObject *Sender)
{
  if (ComboBoxAntibanding->ItemIndex == -1)
	return;

  String antibandingName = (*ComboBoxAntibanding->Items)[ComboBoxAntibanding->ItemIndex];
  for (int i = 0; i < sizeof(Antibandings) / sizeof(Antibandings[0]); ++i)
	if (antibandingName == AntibandingNames[i])
	{
	  FormMain->ACamera->Antibanding = Antibandings[i];
	  break;
	}
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ComboBoxWhiteBalanceChange(TObject *Sender)
{
  if (ComboBoxWhiteBalance->ItemIndex == -1)
	return;

  String whiteBalanceName = (*ComboBoxWhiteBalance->Items)[ComboBoxWhiteBalance->ItemIndex];
  for (int i = 0; i < sizeof(WhiteBalances) / sizeof(WhiteBalances[0]); ++i)
	if (whiteBalanceName == WhiteBalanceNames[i])
	{
	  FormMain->ACamera->WhiteBalance = WhiteBalances[i];
	  break;
	}
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::ApplySettings()
{
  ComboBoxPreviewSizeChange(NULL);
  ComboBoxFocusModeChange(NULL);
  ComboBoxFlashModeChange(NULL);
  ComboBoxColorEffectChange(NULL);
  ComboBoxAntibandingChange(NULL);
  ComboBoxWhiteBalanceChange(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::FormCreate(TObject *Sender)
{
  for (int i = 0; i < FormMain->ACamera->CameraCount; ++i)
  {
	String cameraText = "Camera " + IntToStr(i);

	if (FormMain->ACamera->CameraInfo[i].Facing == cfBack)
	  cameraText += " (Back)";
	else
	  cameraText += " (Front)";

	ComboBoxCamera->Items->Add(cameraText);
  }

  ComboBoxCamera->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFormSettings::CheckBoxStretchToScreenChange(TObject *Sender)
{
  if (CheckBoxStretchToScreen->IsChecked)
	FormMain->Image->WrapMode = TImageWrapMode::Stretch;
  else
	FormMain->Image->WrapMode = TImageWrapMode::Center;
}
//---------------------------------------------------------------------------

