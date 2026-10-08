//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "SmartCoreAI.Comp.Connection"
#pragma link "SmartCoreAI.Driver.Ollama"
#pragma link "SmartCoreAI.Types"
#pragma link "SmartCoreAI.Comp.Chat"
#pragma resource "*.dfm"
TCpp_Ollama_Chat_Florence *Cpp_Ollama_Chat_Florence;
//---------------------------------------------------------------------------
__fastcall TCpp_Ollama_Chat_Florence::TCpp_Ollama_Chat_Florence(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TCpp_Ollama_Chat_Florence::Button1Click(TObject *Sender)
{

		// OBS: Ollama não tem APIKey para acesso.
		if (Edit1->Text.IsEmpty())
			return;

		ActivityMonitor(true);
		Memo1->Lines->Add(L"You: " + Edit1->Text);
		AIChatRequest1->Chat("Historico: " + Memo1->Lines->Text + "Fim histórico " + Edit1->Text);


	Edit1->Text = "";
}
//---------------------------------------------------------------------------
void __fastcall TCpp_Ollama_Chat_Florence::ActivityMonitor(bool AStatus)
 {
	//Efeitos animações
	ActivityIndicator1->Visible = AStatus;
	ActivityIndicator1->Animate = AStatus;
 }
//--------------------------------------------------------------------------
void __fastcall TCpp_Ollama_Chat_Florence::AIChatRequest1Response(TObject *Sender, const UnicodeString Text)

{
   //Tratamento de resposta da IA Ollama
   try {
		//Convertendo resposta em JSON
		std::unique_ptr<TJSONObject> jsonObj(static_cast<TJSONObject*>(
			TJSONObject::ParseJSONValue(Text)
		));

		if (jsonObj) {
			//Se for válido, extrai o JSON pelo campo "response" - mostrando no memo.
			UnicodeString texto = jsonObj->GetValue("response")->Value();
			Memo1->Lines->Add("[Resposta : ]");
			Memo1->Lines->Add(texto);
			Memo1->Lines->Add("");
			Memo1->Lines->Add("------------------------------------------------------------------------------------------------------");
		} else {
			Memo1->Lines->Add("[Resposta : ]");
			Memo1->Lines->Add(Text);
			Memo1->Lines->Add("");
			Memo1->Lines->Add("------------------------------------------------------------------------------------------------------");
		}
	}
	catch (...) {
		//Memo1->Lines->Text = Text;
		Memo1->Lines->Add("[Resposta : ]");
		Memo1->Lines->Add(Text);
		Memo1->Lines->Add("");
		Memo1->Lines->Add("------------------------------------------------------------------------------------------------------");
	}
   ActivityMonitor(false);
}
//---------------------------------------------------------------------------


