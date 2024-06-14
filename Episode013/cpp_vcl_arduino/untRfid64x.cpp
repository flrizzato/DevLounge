//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "untRfid64x.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdPort"
#pragma link "OoMisc"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
  PageControl1-> ActivePage = Scan ;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  PageControl1-> ActivePage = Conf;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  PageControl1-> ActivePage = Usuarios;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  //Imagens- status
  VirtualImage2-> ImageIndex = 10;
  VirtualImage3-> ImageIndex = 2;
  Negado-> Visible = false;
  Autorizado-> Visible = false;



  lblcodId-> Caption= "00000000";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ApdComPort1PortClose(TObject *Sender)
{
  Memo1 -> Lines-> Add("Porta COM fechada");
  VirtualImage2-> ImageIndex = 26;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ApdComPort1PortOpen(TObject *Sender)
{
  Memo1-> Lines-> Add("Porta COM Aberta");
  VirtualImage2-> ImageIndex = 27;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ApdComPort1TriggerAvail(TObject *CP, WORD Count)
{
  fdConsulta->Close();
  //VirtualImage1-> ImageIndex = 20;
  fdUsuario->Close();
  String texto;
  BYTE  input[10];  //vetor de 8 elementos

	 if (Count >= 10){     //quando buffer atingir 8 elementos
						// considerando seu cartão ai

	 ApdComPort1->GetBlock(input,10); // input - passagem do vetor por referencia
								   //o mesmo que passar o ponteiro do primeiro elemento do vetor
								   // 8, o tamanho da memoria do vetor, lê tudo de uma vez só
								   // muito rápido, cuidado para não colocar vaLOR maior que
								   // do vetor, da erro de alocação de memória

									// Só vamos ler 8

	 for (int i = 0; i < 8; i++) {  //percorre todo vetor e lê cada byte pelo indice
	   texto+= char(input[i]);      // Transforma byte em char ASCII e monta a string,
	 }

   //------------Carregando Memo2 cõm informações de TAG--------------------------------------------
	 Memo2->Lines->Add(FormatDateTime("hh:mm:ss",Now()) +" - " +texto);
   //------------Envio de Memo para Label-------------------------------------------------------------
	 lblcodId-> Caption = texto;

  //------------Select de TAG Validada---------------------------------------------------------------
	  String UserCode;
	  UserCode = lblcodId->Caption;

	  fdConsulta-> SQL->Clear();
	  fdConsulta-> SQL-> Text = "Select * from usuario where tagusuario LIKE :pTAG";
	  fdConsulta-> ParamByName("pTAG")-> AsString = UserCode + '%';
	  fdConsulta-> Open();
	  VirtualImage3-> ImageIndex = 1;


	  if (fdConsulta->Fields->Fields[0]->IsNull){
	 //if (fdConsulta->FieldByName("tagusuario")->IsNull) {
		ShowMessage("Sem Cadastro no sistema");
		Autorizado-> Visible = false;
        Negado-> Visible = True;
        fdConsulta-> Close();
		Image1-> Visible = false;


	  }
		 else{
	   ShowMessage("Registrado no Sistema.");
	   Negado-> Visible = false;
	   Autorizado-> Visible = True;
       Image1-> Visible = true;
       fdUsuario-> SQL-> Clear();
	   fdUsuario-> SQL-> Text = "Select * from usuario where tagusuario LIKE :pTAG";
	   fdUsuario-> ParamByName("pTAG")-> AsString = UserCode + '%';
	   fdUsuario-> Open();
	   Image1-> Picture-> LoadFromFile(fdUsuariofotousuario->Text);
	   imgScam-> Picture-> LoadFromFile(fdUsuariofotousuario->Text);

   }
	//-----------Carregando Memo1 com status de conexão----------------------------------------
	   Memo1-> Lines->Add("Reading " + IntToStr(Count) + " bytes");
	}

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button4Click(TObject *Sender)
{
	ApdComPort1-> Open = True;
    ShowMessage("Porta Aberta com sucesso!!");
	fdUsuario-> Close();
	PageControl1-> ActivePage = Scan ;
    VirtualImage2-> ImageIndex = 11;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button5Click(TObject *Sender)
{
  ApdComPort1-> Open = False;
  VirtualImage2-> ImageIndex = 10;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button6Click(TObject *Sender)
{
  Memo1->Lines->Clear();
  Memo2->Lines->Clear();
  lblcodId-> Caption = "";
  fdConsulta-> Close();
  fdUsuario-> Close();
  velhaMarreta(cByte);
}
//---------------------------------------------------------------------------
void    __fastcall TForm1::velhaMarreta(Word count)
{
  BYTE lixo[30];
  ApdComPort1->GetBlock(lixo,count);
}
void __fastcall TForm1::SearchBox1InvokeSearch(TObject *Sender)
{
   fdUsuario-> Close();
  //VirtualImage1-> ImageIndex = 20;
  fdUsuario-> SQL-> Clear();
  fdUsuario-> SQL-> Text = "Select * from usuario where usuario LIKE :pUSU";
  fdUsuario-> ParamByName("pUSU")-> AsString = SearchBox1-> Text + '%';
  fdUsuario-> Open();
  VirtualImage3-> ImageIndex = 1;
  Image1-> Picture-> LoadFromFile(fdUsuariofotousuario->Text);
  imgScam-> Picture-> LoadFromFile(fdUsuariofotousuario->Text);
  lblcodId->Caption= fdUsuariotagusuario->Value;

  if (fdUsuario->Fields->Fields[0]->IsNull){
    ShowMessage("Usuário não Cadastrado!!");
  }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button7Click(TObject *Sender)
{
   if (OpenPictureDialog1-> Execute()) {
	Image1-> Picture-> LoadFromFile(OpenPictureDialog1-> FileName);
	edtEndereco-> Text = OpenPictureDialog1-> FileName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button8Click(TObject *Sender)
{
  fdUsuario-> Open();
  //VirtualImage1-> ImageIndex = 19;
  dtCadastro-> Date = Now();
  fdUsuario-> Insert();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button9Click(TObject *Sender)
{
  fdUsuariofotousuario-> Value = edtEndereco-> Text;
  fdUsuario-> Post();
  fdUsuario-> Close();
  //VirtualImage1-> ImageIndex = 20;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button10Click(TObject *Sender)
{
  fdUsuario-> Edit();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button11Click(TObject *Sender)
{
  fdUsuario-> Delete();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button12Click(TObject *Sender)
{
  Memo2->Lines->SaveToFile("C:\\Users\\thiago.santos\\Desktop\\Migração64X\\txt\\Registro.txt");
  ShowMessage("Arquivo criado com sucesso!!");
}
//---------------------------------------------------------------------------

