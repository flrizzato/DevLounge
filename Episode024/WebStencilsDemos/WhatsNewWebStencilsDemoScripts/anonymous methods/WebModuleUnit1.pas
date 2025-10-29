unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.Generics.Collections, Web.Stencils;

type

  TMap = TDictionary<string, string>;

  TWebModule1 = class(TWebModule)
    WSProcessor: TWebStencilsProcessor;
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LDict: TMap;
begin
  LDict := TMap.Create;
  LDict.Add('APP_VERSION', '1.0.0');
  LDict.Add('DEBUG_MODE', 'True');

  WSProcessor.AddVar('env1', LDict, True,
    function(AVar: TWebStencilsDataVar; const APropName: string; var AValue: string): Boolean
    begin
      Result := TMap(AVar.TheObject).TryGetValue(APropName.ToUpper, AValue);
    end);

  WSProcessor.AddVar('env2', nil, false,
    function (AVar: TWebStencilsDataVar; const APropName: string; var AValue: string): Boolean
    begin
      if APropName.ToUpper = 'APP_VERSION' then
        AValue := '1.0.0'
      else if APropName.ToUpper = 'DEBUG_MODE' then
        Avalue := 'True'
      else
      begin
        Result := False;
        Exit;
      end;
      Result := True;
    end);

  Response.Content := WSProcessor.Content;
end;

end.
