//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Winsoft.FireMonkey.Obr"
#pragma resource "*.fmx"
TFormMain *FormMain;
//---------------------------------------------------------------------------
__fastcall TFormMain::TFormMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormMain::ButtonScanClick(TObject *Sender)
{
  FObr->Active = true;
  FObr->Picture->Assign(ImageControl->Bitmap);
  FObr->Scan();
  if (FObr->BarcodeCount == 0)
	Memo->Lines->Append("No barcode found");
  else
	for (int i = 0; i < FObr->BarcodeCount; ++i)
	{
	  TObrSymbol *barcode = FObr->Barcode[i];
	  Memo->Lines->Append(barcode->SymbologyName + barcode->SymbologyAddonName + " " + barcode->OrientationName + " " + barcode->DataUtf8);
	  Memo->GoToTextEnd();
	}
}
//---------------------------------------------------------------------------

