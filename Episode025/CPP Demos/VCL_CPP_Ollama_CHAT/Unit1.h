//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <System.Skia.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Skia.hpp>
#include <Vcl.WinXCtrls.hpp>
#include "SmartCoreAI.Comp.Connection.hpp"
#include "SmartCoreAI.Driver.Ollama.hpp"
#include "SmartCoreAI.Types.hpp"
#include "SmartCoreAI.Comp.Chat.hpp"
#include <System.IOUtils.hpp>   // para TPath
#include <System.IniFiles.hpp>
#include <Vcl.Imaging.pngimage.hpp>  // para TIniFile
#include <System.JSON.hpp> //Para tratarmos o retorno em JSON


//---------------------------------------------------------------------------
class TCpp_Ollama_Chat_Florence : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TSkLabel *SkLabel1;
	TImage *Image1;
	TPanel *Panel2;
	TEdit *Edit1;
	TButton *Button1;
	TButton *Button2;
	TActivityIndicator *ActivityIndicator1;
	TAIOllamaDriver *AIOllamaDriver;
	TMemo *Memo1;
	TAIConnection *AIConnection1;
	TAIChatRequest *AIChatRequest1;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall AIChatRequest1Response(TObject *Sender, const UnicodeString Text);

private:	// User declarations
    void __fastcall ActivityMonitor(bool AStatus);

public:		// User declarations
	__fastcall TCpp_Ollama_Chat_Florence(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCpp_Ollama_Chat_Florence *Cpp_Ollama_Chat_Florence;
//---------------------------------------------------------------------------
#endif
