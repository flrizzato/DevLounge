//---------------------------------------------------------------------------

#ifndef untRfid64xH
#define untRfid64xH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "AdPort.hpp"
#include "OoMisc.hpp"
#include <System.Skia.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Skia.hpp>
#include <Vcl.ToolWin.hpp>
#include <Vcl.VirtualImage.hpp>
#include <Vcl.WinXCtrls.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.BaseImageCollection.hpp>
#include <Vcl.ImageCollection.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
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
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <FireDAC.VCLUI.Wait.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <Data.Bind.Components.hpp>
#include <Data.Bind.DBScope.hpp>
#include <Data.Bind.EngExt.hpp>
#include <Data.Bind.Grid.hpp>
#include <System.Bindings.Outputs.hpp>
#include <System.Rtti.hpp>
#include <Vcl.Bind.DBEngExt.hpp>
#include <Vcl.Bind.Editors.hpp>
#include <Vcl.Bind.Grid.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtDlgs.hpp>
#include <Vcl.Imaging.pngimage.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TApdComPort *ApdComPort1;
	TPageControl *PageControl1;
	TToolBar *ToolBar1;
	TToolBar *ToolBar2;
	TSkLabel *SkLabel1;
	TButton *Button1;
	TButton *Button2;
	TButton *Button3;
	TVirtualImage *VirtualImage2;
	TSkLabel *SkLabel2;
	TVirtualImage *VirtualImage3;
	TSkLabel *SkLabel3;
	TTabSheet *Scan;
	TTabSheet *Conf;
	TTabSheet *Usuarios;
	TPanel *Panel3;
	TSkAnimatedImage *SkAnimatedImage1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *lblFuncionario;
	TLabel *lblFuncaoCompleto;
	TLabel *lblcodId;
	TLabel *Label6;
	TImage *Image1;
	TSkAnimatedImage *Autorizado;
	TPanel *Panel4;
	TButton *Button4;
	TButton *Button5;
	TButton *Button6;
	TMemo *Memo1;
	TMemo *Memo2;
	TStatusBar *StatusBar1;
	TStatusBar *StatusBar2;
	TPanel *Panel5;
	TLabel *Label5;
	TSearchBox *SearchBox1;
	TStringGrid *StringGrid1;
	TLabel *Label7;
	TLabel *Label8;
	TLabel *Label9;
	TLabel *Label10;
	TLabel *Label11;
	TLabel *Label12;
	TEdit *edtCodusuario;
	TDateTimePicker *dtCadastro;
	TEdit *edtUID;
	TEdit *edtNomeusuario;
	TEdit *edtFuncao;
	TButton *Button7;
	TImage *Image3;
	TEdit *edtEndereco;
	TButton *Button8;
	TButton *Button9;
	TButton *Button10;
	TButton *Button11;
	TPanel *Panel6;
	TImageCollection *ImageCollection1;
	TLabel *Label13;
	TLabel *Label14;
	TFDConnection *FDConnection1;
	TFDQuery *fdConsulta;
	TFDQuery *fdUsuario;
	TFDAutoIncField *fdConsultacodusuario;
	TDateField *fdConsultadtCadastro;
	TWideStringField *fdConsultausuario;
	TWideStringField *fdConsultafuncaousuario;
	TWideStringField *fdConsultatagusuario;
	TStringField *fdConsultafotousuario;
	TFDAutoIncField *fdUsuariocodusuario;
	TDateField *fdUsuariodtCadastro;
	TWideStringField *fdUsuariousuario;
	TWideStringField *fdUsuariofuncaousuario;
	TWideStringField *fdUsuariotagusuario;
	TStringField *fdUsuariofotousuario;
	TFDPhysSQLiteDriverLink *FDPhysSQLiteDriverLink1;
	TBindSourceDB *BindSourceDB1;
	TBindingsList *BindingsList1;
	TLinkGridToDataSource *LinkGridToDataSourceBindSourceDB1;
	TSkAnimatedImage *SkAnimatedImage3;
	TImage *imgScam;
	TOpenPictureDialog *OpenPictureDialog1;
	TLinkPropertyToField *LinkPropertyToFieldCaption;
	TLinkPropertyToField *LinkPropertyToFieldCaption2;
	TImage *Image2;
	TSkAnimatedImage *Negado;
	TButton *Button12;
	TPanel *Panel1;
	TPanel *Panel7;
	TSkAnimatedImage *SkAnimatedImage2;
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ApdComPort1PortClose(TObject *Sender);
	void __fastcall ApdComPort1PortOpen(TObject *Sender);
	void __fastcall ApdComPort1TriggerAvail(TObject *CP, WORD Count);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall SearchBox1InvokeSearch(TObject *Sender);
	void __fastcall Button7Click(TObject *Sender);
	void __fastcall Button8Click(TObject *Sender);
	void __fastcall Button9Click(TObject *Sender);
	void __fastcall Button10Click(TObject *Sender);
	void __fastcall Button11Click(TObject *Sender);
	void __fastcall Button12Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
      Word cByte;
	__fastcall TForm1(TComponent* Owner);
	void    __fastcall velhaMarreta(Word count) ;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
