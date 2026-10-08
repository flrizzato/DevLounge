program ModernShell;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmModernShell},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows Modern');
  Application.Title := 'Modern Shell';
  Application.CreateForm(TfrmModernShell, frmModernShell);
  Application.Run;
end.
