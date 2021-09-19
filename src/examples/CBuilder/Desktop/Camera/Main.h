//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "Winsoft.FireMonkey.Obr.hpp"
#include <FMX.Layouts.hpp>
#include <FMX.Media.hpp>
#include <FMX.Memo.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Objects.hpp>
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:	// IDE-managed Components
	TPanel *PanelBottom;
	TMemo *Memo;
	TPanel *PanelTop;
	TButton *ButtonStartStop;
	TFObr *FObr;
	TCameraComponent *CameraComponent;
	TScrollBox *ScrollBox;
	TImage *Image;
	void __fastcall ButtonStartStopClick(TObject *Sender);
	void __fastcall CameraComponentSampleBufferReady(TObject *Sender, const __int64 ATime);
	void __fastcall FObrBarcodeDetected(TObject *Sender);

private:	// User declarations
	void __fastcall Start();
	void __fastcall Stop();
public:		// User declarations
	__fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
