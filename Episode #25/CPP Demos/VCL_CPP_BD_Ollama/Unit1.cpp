//---------------------------------------------------------------------------

#include <vcl.h>
#include <System.JSON.hpp>
#include <memory>   // para std::unique_ptr
#include <System.RegularExpressions.hpp> //Para remover pontuação


#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "SmartCoreAI.Comp.Chat"
#pragma link "SmartCoreAI.Comp.Connection"
#pragma link "SmartCoreAI.Driver.Ollama"
#pragma link "SmartCoreAI.Types"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
 //InicializarBanco();
}
//---------------------------------------------------------------------------
UnicodeString InterpretarComandoJSON(const UnicodeString& texto)
{
 // Aqui você converte texto simples em JSON esperado pelo ExecutarComandoIA
	// Exemplo simples: apenas cria JSON de SELECT
	TJSONObject *obj = new TJSONObject();
	try
	{
		obj->AddPair("acao", "select");
		obj->AddPair("tabela", "names");

        TJSONArray *campos = new TJSONArray();
        campos->Add("id");
        campos->Add("name");
        obj->AddPair("campos", campos);

        // Opcional: filtro
        if (texto.Pos("id do") > 0)
        {
            obj->AddPair("filtro", "id");
            obj->AddPair("valor", texto.SubString(texto.Pos("id do")+6, texto.Length()).Trim());
        }

        return obj->ToString();
    }
    __finally
    {
        delete obj;
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
	UnicodeString texto = Edit1->Text;
	Label1->Caption = "Interpretando comando...";

	// Chama sua IA (use a sua função existente)
	UnicodeString json = InterpretarComandoJSON(texto);

	ExecutarComandoIA(json);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ExecutarComandoIA(const UnicodeString& jsonCmd)
{
 std::unique_ptr<TJSONValue> val(TJSONObject::ParseJSONValue(jsonCmd));
	if (!val)
	{
		Label1->Caption = "Erro: JSON inválido.";
		return;
	}

	TJSONObject *obj = dynamic_cast<TJSONObject*>(val.get());
	if (!obj) return;

	UnicodeString acao = obj->GetValue("acao")->Value();

	if (acao == "select")
	{
		UnicodeString tabela = obj->GetValue("tabela")->Value();

        // Campos
        UnicodeString camposSQL = "*";
        TJSONArray *arr = dynamic_cast<TJSONArray*>(obj->GetValue("campos"));

        if (arr)
        {
            camposSQL = "";
            for (int i = 0; i < arr->Count; i++)
            {
                if (i > 0) camposSQL += ",";
                camposSQL += arr->Items[i]->Value();
            }
        }

        // Filtro
        TJSONValue *filtroVal = obj->GetValue("filtro");
        TJSONValue *valorVal = obj->GetValue("valor");
        bool temFiltro = (filtroVal && valorVal);

        FDQuery1->Close();
        FDQuery1->SQL->Clear();

        UnicodeString sql = "SELECT " + camposSQL + " FROM " + tabela;

        if (temFiltro)
        {
			sql += " WHERE UPPER(" + filtroVal->Value() + ") LIKE UPPER(:valor)";
        }

        FDQuery1->SQL->Text = sql;

		// Parâmetro
		if (temFiltro)
			FDQuery1->ParamByName("valor")->AsString = "%" + valorVal->Value() + "%";



		FDQuery1->Open();
		ShowMessage("SQL:\n" + FDQuery1->SQL->Text +
			"\n\nParametro = [" + FDQuery1->ParamByName("valor")->AsString + "]");
		Label1->Caption =
			"Carregado: " + tabela + " (" + camposSQL + ")";

		PreencherGrid();
	}

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Edit1KeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
    if (Key == VK_RETURN)
    {
        UnicodeString texto = Edit1->Text;
        Label1->Caption = "Interpretando comando...";
        InterpretarIA(texto);
		Key = 0; // evita beep
		Edit1->Text = "";
	}
}
//---------------------------------------------------------------------------
void TForm1::PreencherGrid()

{
    // Cabeçalho
    StringGrid1->ColCount = FDQuery1->FieldCount;
    for (int c = 0; c < FDQuery1->FieldCount; c++)
        StringGrid1->Cells[c][0] = FDQuery1->Fields->Fields[c]->FieldName;

    // Linhas
    StringGrid1->RowCount = FDQuery1->RecordCount + 1;
    int row = 1;
    FDQuery1->First();
    while (!FDQuery1->Eof)
    {
        for (int c = 0; c < FDQuery1->FieldCount; c++)
            StringGrid1->Cells[c][row] = FDQuery1->Fields->Fields[c]->AsString;
        FDQuery1->Next();
        row++;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
      auto Response = AIChatRequest1->Chat("Olá Célia, você está aí?");
    // A resposta da IA vai disparar AIChatRequest1Response,
	// que chama InterpretarIA(Text)
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AIChatRequest1Response(TObject *Sender, const UnicodeString Text)

{
 InterpretarIA(Text);
}
//---------------------------------------------------------------------------
void TForm1::InterpretarIA(const UnicodeString& MensagemIA)
{
  // 1. Normaliza: minúsculas + remove pontuação
    UnicodeString msg = MensagemIA.LowerCase();
    msg = TRegEx::Replace(msg, "[.,?]", ""); // remove .,?

    // =======================
    // Comando: listar todos os funcionários
    // =======================
    if (msg.Pos("quais funcionarios") > 0 ||
        msg.Pos("quais nomes") > 0 ||
        msg.Pos("listar todos os funcionarios") > 0 ||
        msg.Pos("mostrar todos os nomes") > 0)
    {
        FDQuery1->Close();
        FDQuery1->SQL->Text = "SELECT * FROM Employee";
        FDQuery1->Open();
        PreencherGrid();

        Label1->Caption = "Total de funcionários: " + IntToStr(FDQuery1->RecordCount);
        return;
	}

    	//Comando para Alterar departamento:
	// Comando: alterar departamento
// =======================
if (msg.Pos("mudar departamento") > 0)
{
	UnicodeString nome = ExtrairNome(msg, "departamento");
	UnicodeString novoDepto = ExtrairNovoValor(msg, "departamento");

	if (!nome.IsEmpty() && !novoDepto.IsEmpty())
	{
		try
		{
			FDQuery1->Close();
            FDQuery1->SQL->Text = "UPDATE Employee SET Department = :dep WHERE LOWER(Name) = LOWER(:n)";
			FDQuery1->ParamByName("dep")->AsString = novoDepto;
            FDQuery1->ParamByName("n")->AsString = nome;
            FDQuery1->ExecSQL();

            // Atualiza a grid mostrando o novo valor
            FDQuery1->Close();
            FDQuery1->SQL->Text = "SELECT * FROM Employee";
            FDQuery1->Open();
			PreencherGrid();

            Label1->Caption = "Departamento de " + nome + " alterado para " + novoDepto;
        }
        catch (const Exception &e)
        {
			ShowMessage("Erro ao atualizar: " + e.Message);
        }
    }
    return;
}


    // =======================
    // Comando: buscar departamento
    // =======================
    if (msg.Pos("departamento") > 0)
    {
        UnicodeString nome = ExtrairNome(msg, "departamento");
        if (!nome.IsEmpty())
        {
            FDQuery1->Close();
            FDQuery1->SQL->Text = "SELECT Department FROM Employee WHERE LOWER(Name) = LOWER(:n)";
            FDQuery1->ParamByName("n")->AsString = nome;
            FDQuery1->Open();

            Label1->Caption = "Carregado: Departamento de " + nome;
			PreencherGrid();
		}
		return;
	}




    // =======================
    // Comando: buscar ID
    // =======================
    if (msg.Pos("id") > 0)
    {
        UnicodeString nome = ExtrairNome(msg, "id");
        if (!nome.IsEmpty())
        {
            FDQuery1->Close();
            FDQuery1->SQL->Text = "SELECT ID FROM Employee WHERE LOWER(Name) = LOWER(:n)";
            FDQuery1->ParamByName("n")->AsString = nome;
            FDQuery1->Open();

            Label1->Caption = "Carregado: ID de " + nome;
            PreencherGrid();
        }
        return;
    }

    // =======================
    // Comando: buscar senioridade
    // =======================
    if (msg.Pos("senioridade") > 0 || msg.Pos("anos de experiencia") > 0)
    {
        UnicodeString nome = ExtrairNome(msg, "senioridade");
        if (!nome.IsEmpty())
        {
            FDQuery1->Close();
            FDQuery1->SQL->Text = "SELECT Seniority FROM Employee WHERE LOWER(Name) = LOWER(:n)";
            FDQuery1->ParamByName("n")->AsString = nome;
            FDQuery1->Open();

            Label1->Caption = "Carregado: Senioridade de " + nome;
            PreencherGrid();
        }
        return;
    }

    // =======================
    // Caso não seja comando reconhecido
	// =======================
	ShowMessage("Célia: Não entendi a pergunta. Tente: 'quais funcionários', 'departamento de Alice', 'ID de Bob', 'senioridade de Charles'");
}
//---------------------------------------------------------------------------
UnicodeString TForm1::ExtrairNovoValor(const UnicodeString& msg, const UnicodeString& comando)
{
	// Ex: mudar departamento de Alice para Administrativo"
	// Queremos extrair "Administrativo"
    int posPara = msg.Pos("para");
    if (posPara > 0)
    {
        return msg.SubString(posPara + 4, msg.Length() - (posPara + 3)).Trim();
    }
    return "";
}
//---------------------------------------------------------------------------
UnicodeString TForm1::ExtrairNome(const UnicodeString& msg, const UnicodeString& comando)
{
    // Copia a frase
    UnicodeString frase = msg;

    // Lista de palavras a remover para ficar só o nome
    TStringList *stopWords = new TStringList;
    stopWords->Add("qual");
    stopWords->Add("é");
    stopWords->Add("me");
    stopWords->Add("diga");
    stopWords->Add("mostre");
    stopWords->Add("o");
    stopWords->Add("a");
    stopWords->Add("de");
    stopWords->Add("do");
    stopWords->Add("da");
    stopWords->Add("dos");
    stopWords->Add("das");
    stopWords->Add(comando.LowerCase());

    // Remove as palavras
    for (int i = 0; i < stopWords->Count; i++)
    {
        frase = TRegEx::Replace(frase, "\\b" + stopWords->Strings[i] + "\\b", "", TRegExOptions() << roIgnoreCase);
    }

    frase = frase.Trim(); // Remove espaços extras

    delete stopWords;

    return frase;
}
//---------------------------------------------------------------------------
void TForm1::InicializarBanco()
{
    // Conecta ao banco
    FDConnection1->Connected = true;

    // Cria tabela Employee se não existir
    FDConnection1->ExecSQL(
        "CREATE TABLE IF NOT EXISTS Employee ("
        "ID INTEGER PRIMARY KEY AUTOINCREMENT, "
        "Name TEXT, "
        "Department TEXT, "
        "Seniority INTEGER);"
    );

    // Insere registros iniciais apenas se tabela estiver vazia
    TFDQuery *q = new TFDQuery(NULL);
    try
    {
        q->Connection = FDConnection1;
        q->SQL->Text = "SELECT COUNT(*) AS Total FROM Employee";
        q->Open();
        if (q->FieldByName("Total")->AsInteger == 0)
        {
            FDConnection1->ExecSQL(
                "INSERT INTO Employee (Name, Department, Seniority) VALUES "
                "('Charles', 'TI', 5), "
                "('Felipe', 'Financeiro', 2), "
                "('Thiago', 'RH', 8);"
            );
        }
		q->Close();
    }
    __finally
    {
        delete q;
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  // Conectar FireDAC
    FDConnection1->Connected = false;
    FDConnection1->Params->Database = "EmployeeDB.db";
    FDConnection1->Params->DriverID = "SQLite";
    FDConnection1->Connected = true;

    // Criar tabela Employee se não existir
    FDQuery1->Close();
    FDQuery1->SQL->Text =
        "CREATE TABLE IF NOT EXISTS Employee ("
        "ID INTEGER PRIMARY KEY, "
        "Name TEXT, "
        "Department TEXT, "
        "Seniority INTEGER);";
    FDQuery1->ExecSQL();

    // Inserir dados de teste (evita duplicados)
    FDQuery1->SQL->Text = "INSERT OR IGNORE INTO Employee (ID, Name, Department, Seniority) VALUES (1,'Charles','IT',5)";
    FDQuery1->ExecSQL();
    FDQuery1->SQL->Text = "INSERT OR IGNORE INTO Employee (ID, Name, Department, Seniority) VALUES (2,'Felipe','HR',2)";
    FDQuery1->ExecSQL();
	FDQuery1->SQL->Text = "INSERT OR IGNORE INTO Employee (ID, Name, Department, Seniority) VALUES (3,'Alice','Finance',4)";
	FDQuery1->ExecSQL();

	Label1->Caption = "Conectado e pronto.";
}
//---------------------------------------------------------------------------

