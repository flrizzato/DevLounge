unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.ODBC,
  FireDAC.Phys.ODBCDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.Grids, Vcl.StdCtrls, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, Vcl.Bind.Grid, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope;

type
  TForm1 = class(TForm)
    RadbigqueryConnection: TFDConnection;
    FDQuery1: TFDQuery;
    Button1: TButton;
    StringGrid1: TStringGrid;
    FDQuery1SalesReason: TStringField;
    FDQuery1CustomerName: TStringField;
    FDQuery1Territory: TStringField;
    FDQuery1ShipMethod: TStringField;
    FDQuery1CurrencyCode: TStringField;
    FDQuery1CardType: TStringField;
    FDQuery1City: TStringField;
    FDQuery1State: TStringField;
    FDQuery1PostalCode: TStringField;
    FDQuery1OrderDate: TDateField;
    FDQuery1DueDate: TDateField;
    FDQuery1ShipDate: TDateField;
    FDQuery1OnlineOrderFlag: TLargeintField;
    FDQuery1SubTotal: TFloatField;
    FDQuery1TaxAmount: TFloatField;
    FDQuery1Freight: TFloatField;
    FDQuery1TotalDue: TFloatField;
    FDQuery1SalesOrderID: TLargeintField;
    FDQuery1ProductCategory: TStringField;
    FDQuery1Profit: TFloatField;
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
  FDQuery1.Open;
  for var i:=0 to StringGrid1.ColCount-1 do
    StringGrid1.ColWidths[i] := 125;
end;

end.
