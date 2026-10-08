unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MongoDB,
  FireDAC.Phys.MongoDBDef, System.Rtti, System.JSON.Types, System.JSON.Readers,
  System.JSON.BSON, System.JSON.Builders, FireDAC.Phys.MongoDBWrapper,
  FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Phys.MongoDBDataSet, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    MongoDBConnection: TFDConnection;
    FDMongoQuery1: TFDMongoQuery;
    FDMongoQuery1_id: TWideStringField;
    FDMongoQuery1username: TWideStringField;
    FDMongoQuery1name: TWideStringField;
    FDMongoQuery1address: TWideStringField;
    FDMongoQuery1birthdate: TDateTimeField;
    FDMongoQuery1email: TWideStringField;
    FDMongoQuery1active: TBooleanField;
    FDMongoQuery1accounts: TDataSetField;
    DataSource1: TDataSource;
    FDPhysMongoDriverLink1: TFDPhysMongoDriverLink;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    DBGrid1: TDBGrid;
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
  FDMongoQuery1.Open;
end;

end.
