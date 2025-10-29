program WebBroker;
{$APPTYPE GUI}

uses
  Datasnap.DBClient,
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Web_MainForm in 'Web_MainForm.pas' {Form12},
  Web_WebModule in 'Web_WebModule.pas' {WebModule1: TWebModule},
  Web.Stencils in '..\WebStencilsDemoProject\src\stencils\Web.Stencils.pas',
  Web.StencilsConst in '..\WebStencilsDemoProject\src\stencils\Web.StencilsConst.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
