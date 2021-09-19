//---------------------------------------------------------------------------

#ifndef SettingsH
#define SettingsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Memo.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ScrollBox.hpp>
//---------------------------------------------------------------------------
class TFormSettings : public TForm
{
__published:	// IDE-managed Components
	TButton *ButtonOk;
	TLabel *LabelCamera;
	TComboBox *ComboBoxCamera;
	TLabel *LabelPreviewSize;
	TComboBox *ComboBoxPreviewSize;
	TLabel *LabelFocusMode;
	TComboBox *ComboBoxFocusMode;
	TLabel *LabelFlashMode;
	TComboBox *ComboBoxFlashMode;
	TLabel *LabelColorEffect;
	TComboBox *ComboBoxColorEffect;
	TLabel *LabelAntibanding;
	TComboBox *ComboBoxAntibanding;
	TLabel *LabelWhiteBalance;
	TComboBox *ComboBoxWhiteBalance;
	TLabel *LabelProperties;
	TMemo *MemoProperties;
	TCheckBox *CheckBoxStretchToScreen;
	void __fastcall ButtonOkClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ComboBoxPreviewSizeChange(TObject *Sender);
	void __fastcall ComboBoxCameraChange(TObject *Sender);
	void __fastcall ComboBoxFocusModeChange(TObject *Sender);
	void __fastcall ComboBoxFlashModeChange(TObject *Sender);
	void __fastcall ComboBoxColorEffectChange(TObject *Sender);
	void __fastcall ComboBoxAntibandingChange(TObject *Sender);
	void __fastcall ComboBoxWhiteBalanceChange(TObject *Sender);
	void __fastcall CheckBoxStretchToScreenChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormSettings(TComponent* Owner);
	void __fastcall ApplySettings();

};
//---------------------------------------------------------------------------
extern PACKAGE TFormSettings *FormSettings;
//---------------------------------------------------------------------------
#endif
