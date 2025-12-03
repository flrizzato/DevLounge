program SQLGenerator;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  uMainForm in 'uMainForm.pas' {MainForm};

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
