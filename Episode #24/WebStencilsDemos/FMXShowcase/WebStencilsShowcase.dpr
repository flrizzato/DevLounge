program WebStencilsShowcase;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {WebStencilsLabs},
  PersonClass in 'PersonClass.pas',
  MyTemplates in 'MyTemplates.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWebStencilsLabs, WebStencilsLabs);
  Application.Run;
end.
