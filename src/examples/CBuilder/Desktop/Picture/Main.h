//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "Winsoft.FireMonkey.Obr.hpp"
#include <FMX.Dialogs.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Memo.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ExtCtrls.hpp>
#include <FMX.ScrollBox.hpp>
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelTop;
	TButton *ButtonSelectPicture;
	TPanel *PanelBottom;
	TMemo *Memo;
	TOpenDialog *OpenDialog;
	TFObr *FObr;
	TImageViewer *ImageViewer;
	void __fastcall ButtonSelectPictureClick(TObject *Sender);
	void __fastcall FObrBarcodeDetected(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
