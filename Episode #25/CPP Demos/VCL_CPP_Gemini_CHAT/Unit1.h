//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "SmartCoreAI.Comp.Chat.hpp"
#include "SmartCoreAI.Comp.Connection.hpp"
#include "SmartCoreAI.Driver.Gemini.hpp"
#include "SmartCoreAI.Types.hpp"
#include <Vcl.WinXCtrls.hpp>
#include <System.IniFiles.hpp>
#include <System.IOUtils.hpp>
#include "SmartCoreAI.Driver.Ollama.hpp"
#include <System.Skia.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Skia.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TEdit *Edit1;
	TButton *Button1;
	TMemo *Memo1;
	TAIConnection *AIConnection1;
    TAIGeminiDriver *AIGeminiDriver;
	TAIChatRequest *AIChatRequest1;
	TActivityIndicator *ActivityIndicator1;
	TButton *Button2;
	TPanel *Panel1;
	TSkLabel *SkLabel1;
	TImage *Image1;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall AIChatRequest1Response(TObject *Sender, const UnicodeString Text);
	void __fastcall FormResize(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);

private:	// User declarations
	 TGUID FlastRequestId;
	 void __fastcall ActivityMonitor(bool AStatus);
     bool EnsureApiKeyFromIni(System::UnicodeString &AApiKey,
							 const System::UnicodeString ASection = L"AI",
                             const System::UnicodeString AIdent = L"API_KEY",
							 const System::UnicodeString AIniPath = L"");
 public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
