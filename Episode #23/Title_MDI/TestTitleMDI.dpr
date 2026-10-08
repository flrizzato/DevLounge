program TestTitleMDI;

uses
  Vcl.Forms,
  Vcl.Themes,
  TestMain in 'TestMain.pas' {frmTest},
  TestChild in 'TestChild.pas' {TestChild},
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTest, frmTest);
  Application.Run;
end.
