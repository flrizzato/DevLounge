program NavViewAppShell;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmNavViewAppShell},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows Modern');
  Application.Title := 'TNavigationView - App Shell';
  Application.CreateForm(TfrmNavViewAppShell, frmNavViewAppShell);
  Application.Run;
end.
