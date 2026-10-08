unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.Generics.Collections, Web.Stencils;

type
  TWebModule1 = class(TWebModule)
    WebSessionManager: TWebSessionManager;
    WebFormsAuthenticator: TWebFormsAuthenticator;
    WebAuthorizer: TWebAuthorizer;
    WSEngine: TWebStencilsEngine;
    WebFileDispatcher1: TWebFileDispatcher;
    procedure WebFormsAuthenticatorAuthenticate(Sender: TCustomWebAuthenticator;
      Request: TWebRequest; const UserName, Password: string; var Roles: string;
      var Success: Boolean);
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

procedure TWebModule1.WebFormsAuthenticatorAuthenticate(
  Sender: TCustomWebAuthenticator; Request: TWebRequest; const UserName,
  Password: string; var Roles: string; var Success: Boolean);
begin
  // Demo hardcoded credentials
  Success := False;
  Roles := '';
  if SameText(UserName, 'demo') and SameText(Password, 'demo123') then
  begin
    Success := True;
    Roles := 'user';
  end
  else if SameText(UserName, 'admin') and SameText(Password, 'admin123') then
  begin
    Success := True;
    Roles := 'admin';
  end;
end;

end.
