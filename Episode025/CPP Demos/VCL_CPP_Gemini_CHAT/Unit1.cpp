#include <vcl.h>
#pragma hdrstop
#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "SmartCoreAI.Comp.Chat"
#pragma link "SmartCoreAI.Comp.Connection"
#pragma link "SmartCoreAI.Driver.Gemini"
#pragma link "SmartCoreAI.Types"
#pragma link "SmartCoreAI.Driver.Ollama"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
	AIGeminiDriver = new Smartcoreai::Driver::Gemini::TAIGeminiDriver(this); // associa ao formulário, criando e fixando o drive do Gemini.
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  System::UnicodeString LAPIKey;

    if (EnsureApiKeyFromIni(LAPIKey))
    {
		// Define a API Key do Gemini
        Smartcoreai::Driver::Gemini::TAIGeminiParams *Params =
			dynamic_cast<Smartcoreai::Driver::Gemini::TAIGeminiParams*>(AIGeminiDriver->Params);

        if (Params)
            Params->APIKey = LAPIKey;

		if (Edit1->Text.IsEmpty())
            return;

		ActivityMonitor(true);
		Memo1->Lines->Add(L"You: " + Edit1->Text);
		AIChatRequest1->Chat("Historico: " + Memo1->Lines->Text + "Fim histórico " + Edit1->Text);
	}

	Edit1->Text = "";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AIChatRequest1Response(TObject *Sender, const UnicodeString Text)
{
  //Obtendo resposta do texto - enviado pelo Gemini em Text
  Memo1->Lines->Add("[Resposta :]");
  Memo1->Lines->Add(Text);
  ActivityMonitor(false);
  Memo1->Lines->Add("-------------------------------------------------------------------------------------------------------------------");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
  //Posição do Incicator
  ActivityIndicator1->Left = (ClientWidth  - ActivityIndicator1->Width)  / 2;
  ActivityIndicator1->Top  = (ClientHeight - ActivityIndicator1->Height) / 2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ActivityMonitor(bool AStatus)
 {
	//Efeito visual
	ActivityIndicator1->Visible = AStatus;
	ActivityIndicator1->Animate = AStatus;
 }
//---------------------------------------------------------------------------
bool  TForm1::EnsureApiKeyFromIni(System::UnicodeString &AApiKey,
												  const System::UnicodeString ASection,
												  const System::UnicodeString AIdent,
												  const System::UnicodeString AIniPath)
{

	//Criei uma função membro para criar a APIKEY em um arquivo .ini
	bool Result = false;

	// Chave já configurada no driver
	Smartcoreai::Driver::Gemini::TAIGeminiParams *Params =
		dynamic_cast<Smartcoreai::Driver::Gemini::TAIGeminiParams*>(AIGeminiDriver->Params);

	if (Params && !Params->APIKey.IsEmpty())
	{
		AApiKey = Params->APIKey;
		return true;
	}

	// Definindo nome do arquivo INI
	System::UnicodeString IniFileName = AIniPath;
	if (IniFileName.IsEmpty())
		IniFileName = TPath::ChangeExtension(ParamStr(0), L".ini");

	// Le ou cria o .INI
	std::unique_ptr<TIniFile> Ini(new TIniFile(IniFileName));

	System::UnicodeString Tmp = Ini->ReadString(ASection, AIdent, L"").Trim();

	// Se não encotrar pergunta ao usuário
	while (Tmp.IsEmpty())
	{
		if (!InputQuery(L"Por favor entre com sua API Key", L"API Key", Tmp))
			return false; // usuário cancelou
		Tmp = Tmp.Trim();
	}

	// Se for novo, ele atualiza.
	if (Ini->ReadString(ASection, AIdent, L"") != Tmp)
		Ini->WriteString(ASection, AIdent, Tmp);

	// Retorno APIKey.
	AApiKey = Tmp;
	Result = true;

	return Result;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
	  if (!IsEqualGUID(FlastRequestId, GUID_NULL))
	{
		AIGeminiDriver->Cancel(FlastRequestId);
	}
}
//---------------------------------------------------------------------------
