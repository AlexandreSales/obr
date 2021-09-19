//---------------------------------------------------------------------------
#include <fmx.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//#pragma link "Winsoft.FireMonkey.Obr"
#pragma resource "*.fmx"
TFormMain *FormMain;
//---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::Start()
{
  FObr->Active = true;
  CameraComponent->Active = true;
  ButtonStartStop->Text = "Stop";
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::Stop()
{
  CameraComponent->Active = false;
  FObr->Active = false;
  ButtonStartStop->Text = "Start";
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::ButtonStartStopClick(TObject *Sender)
{
  try
  {
	if (!FObr->Active)
	  Start();
	else
	  Stop();
  }
  catch (Exception &e)
  {
	Stop();
	throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::CameraComponentSampleBufferReady(TObject *Sender, const __int64 ATime)
{
  CameraComponent->SampleBufferToBitmap(Image->Bitmap, true);
  CameraComponent->SampleBufferToBitmap(FObr->Picture, true);
  FObr->Scan();
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FObrBarcodeDetected(TObject *Sender)
{
  for (int i = 0; i < FObr->BarcodeCount; ++i)
  {
	TObrSymbol *barcode = FObr->Barcode[i];
	String line = barcode->SymbologyName + barcode->SymbologyAddonName + " " + barcode->OrientationName + " " + barcode->DataUtf8;
	if (i < Memo->Lines->Count)
	  Memo->Lines->Strings[i] = line;
	else
	  Memo->Lines->Add(line);
  }

  while (Memo->Lines->Count > FObr->BarcodeCount)
	Memo->Lines->Delete(Memo->Lines->Count - 1);
}
//---------------------------------------------------------------------------

