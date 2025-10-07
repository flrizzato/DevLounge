unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
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
  var
  strJSON := '''
    [
      { "id" : "1", "name" : "Large" } ,
      { "id" : "2", "name" : "Medium" } ,
      { "id" : "2", "name" : "Small" }
    ]
    ''';

  var
  strSQL := '''
      SELECT *
      FROM Customers
      WHERE Department = 'R&D'
      ORDER BY Name;
      ''';


   // The string below is invalid: there is text before the new line
//   var
//   strInvalidString := '''SELECT *
//     FROM Customers
//     WHERE Department = 'R&D'
//     ORDER BY Name;
//     ''';


end;

end.
