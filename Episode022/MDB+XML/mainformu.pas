unit mainformu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.DBCtrls, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls, Vcl.Buttons;

type
  TMainForm = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses datamodu;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  {$IFDEF WIN32}
  var fMsg := '[32 bit]';
  {$ENDIF}
  {$IFDEF WIN64}
  var fMsg := '[64 bit]';
  {$ENDIF}

  MainDM.MSAccessCnn.Close;
  MainDM.MSAccessCnn.Open;
  if MainDM.MSAccessCnn.Connected then
    ShowMessage('Connected to a MSAccess from a ' + fMsg + ' Windows application!');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DataSource1.DataSet.Open;
end;

end.
