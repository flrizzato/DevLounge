unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Skia, Vcl.StdCtrls, Skia.Vcl;

type
  TForm1 = class(TForm)
    SkAnimatedImage1: TSkAnimatedImage;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  SkAnimatedImage1.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SkAnimatedImage1.Enabled := False;
end;

end.
