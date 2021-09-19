//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include "Winsoft.Android.Camera.hpp"
#include <FMX.ExtCtrls.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <Androidapi.JNI.GraphicsContentViewText.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Objects.hpp>
#include "Winsoft.FireMonkey.Obr.hpp"
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:	// IDE-managed Components
	TImage *Image;
	TButton *ButtonSettings;
	TButton *ButtonStartStop;
	TACamera *ACamera;
	TFObr *FObr;
	TLabel *LabelBarcode;
	void __fastcall ACameraDataReady(TObject *Sender);
	void __fastcall ButtonSettingsClick(TObject *Sender);
	void __fastcall ButtonStartStopClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	_di_IFMXApplicationEventService FMXApplicationEventService;
	bool Paused;
	int RgbaDataWidth;
	int RgbaDataHeight;
	DynamicArray<Cardinal> RgbaData;
	void __fastcall ProcessData();
	bool __fastcall HandleAppEvent(TApplicationEvent AAppEvent, TObject *AContext);
public:		// User declarations
	__fastcall TFormMain(TComponent* Owner);
	void __fastcall Start();
	void __fastcall Stop();
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
