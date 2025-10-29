unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.Generics.Collections, Web.Stencils;

type

  TStatusObject = class
  private
    function RandomStatus: string;
  public
    property Name: string read RandomStatus;
  end;

  TWebModule1 = class(TWebModule)
    WSProcessor: TWebStencilsProcessor;
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FStatus: TStatusObject;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

function TStatusObject.RandomStatus: string;
const
  Statuses: array[0..4] of string = ('a', 'i', 'p', 's', 'm');
begin
  Result := Statuses[Random(5)];
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
    Response.Content := WSProcessor.Content;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FStatus := TStatusObject.Create;
  WSProcessor.AddVar('status', FStatus, False);
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  FStatus.Free;
end;

end.
