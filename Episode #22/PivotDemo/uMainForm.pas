unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Phys.SQLiteWrapper.Stat,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt, Vcl.Bind.Grid, System.Rtti,
  System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, Vcl.Grids, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.SQLiteVDataSet, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    FDSQLite: TFDConnection;
    FDLocalSQL: TFDLocalSQL;
    FDQueryPivot: TFDQuery;
    FDInterbase: TFDConnection;
    FDOrders: TFDQuery;
    FDOrdersORDERNO: TFloatField;
    FDOrdersCUSTNO: TFloatField;
    FDOrdersSALEDATE: TSQLTimeStampField;
    FDOrdersSHIPDATE: TSQLTimeStampField;
    FDOrdersEMPNO: TIntegerField;
    FDOrdersSHIPTOCONTACT: TStringField;
    FDOrdersSHIPTOADDR1: TStringField;
    FDOrdersSHIPTOADDR2: TStringField;
    FDOrdersSHIPTOCITY: TStringField;
    FDOrdersSHIPTOSTATE: TStringField;
    FDOrdersSHIPTOZIP: TStringField;
    FDOrdersSHIPTOCOUNTRY: TStringField;
    FDOrdersSHIPTOPHONE: TStringField;
    FDOrdersSHIPVIA: TStringField;
    FDOrdersPO: TStringField;
    FDOrdersTERMS: TStringField;
    FDOrdersPAYMENTMETHOD: TStringField;
    FDOrdersITEMSTOTAL: TFloatField;
    FDOrdersTAXRATE: TFloatField;
    FDOrdersFREIGHT: TFloatField;
    FDOrdersAMOUNTPAID: TFloatField;
    StringGrid1: TStringGrid;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDCustomer: TFDQuery;
    FDCustomerCUSTNO: TFloatField;
    FDCustomerCOMPANY: TStringField;
    FDCustomerADDR1: TStringField;
    FDCustomerADDR2: TStringField;
    FDCustomerCITY: TStringField;
    FDCustomerSTATE: TStringField;
    FDCustomerZIP: TStringField;
    FDCustomerCOUNTRY: TStringField;
    FDCustomerPHONE: TStringField;
    FDCustomerFAX: TStringField;
    FDCustomerTAXRATE: TFloatField;
    FDCustomerCONTACT: TStringField;
    FDCustomerLASTINVOICEDATE: TSQLTimeStampField;
    BindSourceDB2: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource;
    Button1: TButton;
    FDQueryPivotCUSTNO: TFloatField;
    FDQueryPivotCOMPANY: TStringField;
    FDQueryPivotSALE_YEAR: TWideStringField;
    FDQueryPivotSALES_PER_YEAR: TFloatField;
    FDQueryPivotTOTAL_SALES: TFloatField;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FDInterbase.Open;
  FDCustomer.Active := True;
  FDOrders.Active := True;

  FDSQLite.Open;
  FDLocalSQL.Active := True;
  FDQueryPivot.Active := True;
end;

end.
