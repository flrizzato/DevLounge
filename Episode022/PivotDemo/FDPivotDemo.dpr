program FDPivotDemo;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows11 Impressive Dark SE');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
