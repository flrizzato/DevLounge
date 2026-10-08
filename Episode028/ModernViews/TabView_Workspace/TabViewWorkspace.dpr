program TabViewWorkspace;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmTabViewWorkspace},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows Modern');
  Application.Title := 'TTabView - Workspace';
  Application.CreateForm(TfrmTabViewWorkspace, frmTabViewWorkspace);
  Application.Run;
end.
