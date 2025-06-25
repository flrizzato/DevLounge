unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.Oracle,
  FireDAC.Phys.OracleDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Vcl.Bind.Grid,
  System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, Vcl.Grids, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    T1ixblbn7708yv6x_tpConnection: TFDConnection;
    CustomersTable: TFDQuery;
    CustomersTableCUST_ID: TFMTBCDField;
    CustomersTableCUST_FIRST_NAME: TStringField;
    CustomersTableCUST_LAST_NAME: TStringField;
    CustomersTableCUST_GENDER: TStringField;
    CustomersTableCUST_YEAR_OF_BIRTH: TBCDField;
    CustomersTableCUST_MARITAL_STATUS: TStringField;
    CustomersTableCUST_STREET_ADDRESS: TStringField;
    CustomersTableCUST_POSTAL_CODE: TStringField;
    CustomersTableCUST_CITY: TStringField;
    CustomersTableCUST_CITY_ID: TFMTBCDField;
    CustomersTableCUST_STATE_PROVINCE: TStringField;
    CustomersTableCUST_STATE_PROVINCE_ID: TFMTBCDField;
    CustomersTableCOUNTRY_ID: TFMTBCDField;
    CustomersTableCUST_MAIN_PHONE_NUMBER: TStringField;
    CustomersTableCUST_INCOME_LEVEL: TStringField;
    CustomersTableCUST_CREDIT_LIMIT: TFMTBCDField;
    CustomersTableCUST_EMAIL: TStringField;
    CustomersTableCUST_TOTAL: TStringField;
    CustomersTableCUST_TOTAL_ID: TFMTBCDField;
    CustomersTableCUST_SRC_ID: TFMTBCDField;
    CustomersTableCUST_EFF_FROM: TDateTimeField;
    CustomersTableCUST_EFF_TO: TDateTimeField;
    CustomersTableCUST_VALID: TStringField;
    Button1: TButton;
    StringGrid1: TStringGrid;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
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
  CustomersTable.Open;
end;

end.
