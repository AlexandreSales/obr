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
void __fastcall TFormMain::ButtonSelectPictureClick(TObject *Sender)
{
	if (OpenDialog->Execute())
	{
	  Memo->Lines->Clear();
	  ImageViewer->Bitmap->LoadFromFile(OpenDialog->FileName);

	  FObr->Active = true;
	  try
	  {
		FObr->Picture->Assign(ImageViewer->Bitmap);
		FObr->Scan();
		if (FObr->BarcodeCount == 0)
		  Memo->Lines->Append("No barcode found");
	  }
	  __finally
	  {
		FObr->Active = false;
		FObr->Picture = NULL;
	  }
	}
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::FObrBarcodeDetected(TObject *Sender)
{
  Memo->Lines->Clear();
  for (int i = 0; i < FObr->BarcodeCount; ++i)
  {
	TObrSymbol *barcode = FObr->Barcode[i];
	Memo->Lines->Append(barcode->SymbologyName + barcode->SymbologyAddonName + " " + barcode->OrientationName + " " + barcode->DataUtf8);
  }
}
//---------------------------------------------------------------------------

