program TestTitleActionToolBar;

uses
  Vcl.Forms,
  Vcl.Themes,
  TestMain in 'TestMain.pas' {frmTest},
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTest, frmTest);
  Application.Run;
end.
