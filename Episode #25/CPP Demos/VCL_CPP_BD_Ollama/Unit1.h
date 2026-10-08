//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.Grid.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Phys.SQLite.hpp>
#include <FireDAC.Phys.SQLiteDef.hpp>
#include <FireDAC.Phys.SQLiteWrapper.Stat.hpp>
#include <FireDAC.Stan.Async.hpp>
#include <FireDAC.Stan.Def.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.ExprFuncs.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <FireDAC.VCLUI.Wait.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <Vcl.Bind.DBEngExt.hpp>
#include <Vcl.Bind.Editors.hpp>
#include <Vcl.Bind.Grid.hpp>
#include <Vcl.Grids.hpp>
#include "SmartCoreAI.Comp.Chat.hpp"
#include "SmartCoreAI.Comp.Connection.hpp"
#include "SmartCoreAI.Driver.Ollama.hpp"
#include "SmartCoreAI.Types.hpp"
//#include "SmartCoreAI.Chat.hpp"
#undef SendMessage


UnicodeString InterpretarComandoJSON(const UnicodeString& texto);

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TEdit *Edit1;
	TButton *Button1;
	TLabel *Label1;
	TStringGrid *StringGrid1;
	TFDConnection *FDConnection1;
	TBindSourceDB *BindSourceDB1;
	TBindingsList *BindingsList1;
	TAIConnection *AIConnection1;
	TAIChatRequest *AIChatRequest1;
	TAIOllamaDriver *AIOllamaDriver;
	TButton *Button2;
	TFDQuery *FDQuery1;
	TBindSourceDB *BindSourceDB2;
	TLinkGridToDataSource *LinkGridToDataSourceBindSourceDB2;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Edit1KeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall AIChatRequest1Response(TObject *Sender, const UnicodeString Text);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	void ExecutarComandoIA(const UnicodeString& jsonCmd);
	void PreencherGrid();
	void InterpretarIA(const UnicodeString& MensagemIA);
	void InicializarBanco();
	UnicodeString ExtrairNome(const UnicodeString& msg, const UnicodeString& comando);
    UnicodeString ExtrairNovoValor(const UnicodeString& msg, const UnicodeString& comando);

	public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
