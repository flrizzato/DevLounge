program TeraData101;

uses
  Vcl.Forms,
  FireDAC.Phys.TData in 'lib\FireDAC.Phys.TData.pas',
  uMainForm in 'uMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
