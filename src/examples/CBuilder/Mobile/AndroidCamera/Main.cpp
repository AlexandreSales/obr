//---------------------------------------------------------------------------
#include <fmx.h>
#pragma hdrstop

#include "Main.h"
#include "Settings.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Winsoft.Android.Camera"
#pragma link "Winsoft.FireMonkey.Obr"
#pragma resource "*.fmx"
TFormMain *FormMain;
//---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
int Processing = 0;

void __fastcall TFormMain::ACameraDataReady(TObject *Sender)
{
  if (Processing == 0)
  {
	++Processing;
	ACamera->DataToRgba(&RgbaData[0], ACamera->PreviewRotation, ACamera->PreviewHorizMirror);
	TThread::Queue(NULL, ProcessData);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::ProcessData()
{
  TBitmap *bitmap = new TBitmap();

  if (ACamera->PreviewRotation == ro0 || ACamera->PreviewRotation == ro180)
	ACamera->UpdateBitmap(bitmap, &RgbaData[0], RgbaDataWidth, RgbaDataHeight);
  else
	ACamera->UpdateBitmap(bitmap, &RgbaData[0], RgbaDataHeight, RgbaDataWidth);

  __try
  {
	  FObr->Picture->Assign(bitmap);
	  FObr->Scan();
	  if (FObr->BarcodeCount > 0)
	  {
		TObrSymbol *barcode = FObr->Barcode[0];
		LabelBarcode->Text = barcode->SymbologyName + barcode->SymbologyAddonName + " " + barcode->OrientationName + " " + barcode->DataUtf8;
		if (barcode->LocationCount >= 2)
		{
		  bitmap->Canvas->BeginScene();
		  __try
		  {
			TPathData *path = new TPathData();
			path->MoveTo(TPointF(barcode->LocationX[barcode->LocationCount - 1], barcode->LocationY[barcode->LocationCount - 1]));
			for (int i = 0; i < barcode->LocationCount; ++i)
			  path->LineTo(TPointF(barcode->LocationX[i], barcode->LocationY[i]));

			bitmap->Canvas->Stroke->Color = (TAlphaColor)TAlphaColorRec::Red;
			bitmap->Canvas->DrawPath(path, 100);
		  }
		  __finally
		  {
			bitmap->Canvas->EndScene();
		  }
		}
	  }
  }
  __finally
  {
	Image->Bitmap = bitmap;
	--Processing;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::ButtonSettingsClick(TObject *Sender)
{
  Stop();
  FormSettings->Show();
  ButtonStartStop->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::Start()
{
  if (ACamera->PreviewFormat != pfNV21)
  {
	ACamera->PreviewFormat = pfNV21;
	if (ACamera->PreviewFormat != pfNV21)
	  throw new Exception("Preview format has to be NV21");
  }

  RgbaDataWidth = ACamera->PreviewSize.Width;
  RgbaDataHeight = ACamera->PreviewSize.Height;
  RgbaData.Length = RgbaDataWidth * RgbaDataHeight;

  FObr->Active = true;
  ACamera->Start();
  ButtonStartStop->Text = "Stop";
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::Stop()
{
  ACamera->Stop();
  ButtonStartStop->Text = "Start";
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::ButtonStartStopClick(TObject *Sender)
{
  if (ACamera->Capturing)
	Stop();
  else
	Start();
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FormCreate(TObject *Sender)
{
  if (TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXScreenService)))
  {
	FMXApplicationEventService = TPlatformServices::Current->GetPlatformService(__uuidof(IFMXScreenService));
	if (FMXApplicationEventService)
	  FMXApplicationEventService->SetApplicationEventHandler(HandleAppEvent);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFormMain::HandleAppEvent(TApplicationEvent AAppEvent, TObject *AContext)
{
  switch (AAppEvent)
  {
	case TApplicationEvent::EnteredBackground:
	{
	  if (ACamera->Capturing)
		Paused = true;
	  Stop();
	  ACamera->Active = false;
	  break;
	}

	case TApplicationEvent::WillBecomeForeground:
	{
	  ACamera->Active = true;
	  FormSettings->ApplySettings();
	  if (Paused)
	  {
		Start();
		Paused = false;
	  }
	  break;
	}

	default:
	  break;
  }

  return true;
}
//---------------------------------------------------------------------------
