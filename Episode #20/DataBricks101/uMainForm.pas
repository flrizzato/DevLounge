unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.ODBC,
  FireDAC.Phys.ODBCDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.Bind.EngExt, Vcl.Bind.DBEngExt,
  Vcl.Bind.Grid, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    RaddatabricksConnection: TFDConnection;
    FDQuery1: TFDQuery;
    FDQuery1address_id: TBCDField;
    FDQuery1address_type_code: TStringField;
    FDQuery1address_line_1: TStringField;
    FDQuery1address_line_2: TStringField;
    FDQuery1address_line_3: TStringField;
    FDQuery1address_line_4: TStringField;
    FDQuery1customer_code: TStringField;
    FDQuery1city_code: TStringField;
    FDQuery1city_name: TStringField;
    FDQuery1state_code: TStringField;
    FDQuery1zip_code: TStringField;
    FDQuery1county_name: TStringField;
    FDQuery1country_code: TStringField;
    FDQuery1area_code: TStringField;
    FDQuery1phone_number: TStringField;
    FDQuery1primary_contact_person: TStringField;
    FDQuery1secondary_contact_person: TStringField;
    FDQuery1creating_employee_id: TBCDField;
    FDQuery1created_datetime: TSQLTimeStampField;
    FDQuery1last_change_employee_id: TBCDField;
    FDQuery1last_change_datetime: TSQLTimeStampField;
    FDQuery1dss_record_source: TStringField;
    FDQuery1dss_load_date: TSQLTimeStampField;
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
