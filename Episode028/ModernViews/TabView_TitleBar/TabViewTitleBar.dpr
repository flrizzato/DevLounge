program TabViewTitleBar;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmTabViewTitleBar},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows Modern');
  Application.Title := 'TTabView - Title Bar';
  Application.CreateForm(TfrmTabViewTitleBar, frmTabViewTitleBar);
  Application.Run;
end.
