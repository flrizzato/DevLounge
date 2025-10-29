program TemplateDemo;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  TemplateDemo_Form in 'TemplateDemo_Form.pas' {Form1} ,
  TemplateDemo_WebModule in 'TemplateDemo_WebModule.pas' {WebModule1: TWebModule};

{$R *.res}

begin
  if WebRequestHandler <> nil
  then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm( TForm1, Form1 );
  Application.Run;

end.
