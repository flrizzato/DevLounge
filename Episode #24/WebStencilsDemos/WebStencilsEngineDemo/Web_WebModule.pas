unit Web_WebModule;

interface

uses
  SysUtils, Classes, HTTPApp, Web.Stencils, Generics.Collections, DB, DBClient;

type
  TWebModule1 = class(TWebModule)
    WebStencilsEngine1: TWebStencilsEngine;
    ClientDataSet1: TClientDataSet;
    WebFileDispatcher1: TWebFileDispatcher;
    procedure WebModuleCreate(Sender: TObject);
    procedure WebStencilsEngine1PathTemplates8PathInit(Sender: TObject;
      const ARequest: TWebPostProcessorRequest);
    procedure WebStencilsEngine1Value(Sender: TObject; const AObjectName, APropName: string; var AReplaceText: string;
      var AHandled: Boolean);
    procedure WebStencilsEngine1Language(Sender: TObject; const APropName: string; var AReplaceText: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

{ TSimpleObject }

type
  TSimpleObject = class
  private
    FName: string;
    FValue: Integer;
    function GetValueBelowTen: Boolean;
  public
    constructor Create(AName: string; AValue: Integer);
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
    property ValueBelowTen: Boolean read GetValueBelowTen;
  end;

constructor TSimpleObject.Create(AName: string; AValue: Integer);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
end;

function TSimpleObject.GetValueBelowTen: Boolean;
begin
  Result := Value < 10;
end;

{ TWebModule1 }

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  WebStencilsEngine1.AddVar('dataset', ClientDataSet1, False);
  WebStencilsEngine1.AddVar('dataO', TSimpleObject.Create('joe', 55));
  WebStencilsEngine1.AddVar('obj1', TSimpleObject.Create('joe', 55));
  WebStencilsEngine1.AddVar('obj2', TSimpleObject.Create('marc', 9));
end;

procedure TWebModule1.WebStencilsEngine1Language(Sender: TObject; const APropName: string; var AReplaceText: string);
begin
  if APropName = 'hellow' then
    AReplaceText := 'Hello World from Engine';
end;

procedure TWebModule1.WebStencilsEngine1PathTemplates8PathInit(Sender: TObject;
  const ARequest: TWebPostProcessorRequest);
begin
  var id := ARequest.Vars.Values['id'];
  if id <> '' then
    ClientDataSet1.Locate('CustNo', id, []);
end;

procedure TWebModule1.WebStencilsEngine1Value(Sender: TObject; const AObjectName, APropName: string;
  var AReplaceText: string; var AHandled: Boolean);
begin
  if SameText(AObjectName, 'dataE') and
     (SameText(APropName, 'name') or SameText(APropName, 'value')) then
  begin
    AHandled := True;
    AReplaceText := 'You requested ' + APropName;
  end;
end;

end.
