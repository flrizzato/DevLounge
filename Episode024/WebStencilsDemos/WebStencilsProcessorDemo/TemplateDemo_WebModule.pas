unit TemplateDemo_WebModule;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  Data.DB,
  Datasnap.DBClient,
  Web.Stencils;

type
  TWebModule1 = class( TWebModule )
    ProcessorHome : TWebStencilsProcessor;
    ClientDataSet1 : TClientDataSet;
    ProcessorCompanyList : TWebStencilsProcessor;
    ProcessorCompany : TWebStencilsProcessor;
    ProcessorGeneric : TWebStencilsProcessor;
    WebStencilsEngine1 : TWebStencilsEngine;
    ProcessorFilename : TWebStencilsProcessor;
    procedure ProcessorHomeValue( Sender : TObject; const AObjectName, APropName : string; var AReplaceText : string;
      var AHandled : Boolean );
    procedure WebModule1WebActionItem1Action( Sender : TObject;
      Request : TWebRequest; Response : TWebResponse; var Handled : Boolean );
    procedure WebModule1ActionCompanyAction( Sender : TObject;
      Request : TWebRequest; Response : TWebResponse; var Handled : Boolean );
    procedure WebModule1ActionIfAction( Sender : TObject; Request : TWebRequest;
      Response : TWebResponse; var Handled : Boolean );
    procedure ProcessorCompanyScaffolding( Sender : TObject;
      const AQualifClassName : string; var AReplaceText : string );
    procedure WebStencilsEngine1Language( Sender : TObject; const APropName : string; var AReplaceText : string );
    procedure WebModule1ActionGetValueFromObjectAction( Sender : TObject; Request : TWebRequest;
      Response : TWebResponse;
      var Handled : Boolean );
    procedure WebModule1ActionGetValueFromEventAction( Sender : TObject; Request : TWebRequest; Response : TWebResponse;
      var Handled : Boolean );
    procedure ProcessorGenericValue( Sender : TObject; const AObjectName, APropName : string; var AReplaceText : string;
      var AHandled : Boolean );
    procedure WebStencilsEngine1Value( Sender : TObject; const AObjectName, APropName : string;
      var AReplaceText : string;
      var AHandled : Boolean );
    private
      { Private declarations }
    public
      { Public declarations }
  end;

var
  WebModuleClass : TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

type
  TSimpleObject = class
    private
      FName : string;
      FValue : Integer;
      procedure SetName( const Value : string );
      procedure SetValue( const Value : Integer );
      function GetValueBelowTen : Boolean;
    public
      constructor Create( aName : string; aValue : Integer );
      property Name : string
        read FName
        write SetName;
      property Value : Integer
        read FValue
        write SetValue;
      property ValueBelowTen : Boolean
        read GetValueBelowTen;
  end;

  { TSimpleObject }

constructor TSimpleObject.Create( aName : string; aValue : Integer );
begin
  inherited Create;
  FName := aName;
  FValue := aValue;
end;

function TSimpleObject.GetValueBelowTen : Boolean;
begin
  Result := FValue < 10;
end;

procedure TSimpleObject.SetName( const Value : string );
begin
  FName := Value;
end;

procedure TSimpleObject.SetValue( const Value : Integer );
begin
  FValue := Value;
end;

procedure TWebModule1.ProcessorCompanyScaffolding( Sender : TObject;
  const AQualifClassName : string; var AReplaceText : string );
begin
  if SameText( AQualifClassName, 'Company' )
  then
  begin
    AReplaceText := '';
    for var I := 0 to ClientDataSet1.Fields.Count - 1 do
      AReplaceText := AReplaceText + '<p>' + ClientDataSet1.Fields[ I ].FieldName + ': @dataset.' +
        ClientDataSet1.Fields[ I ].FieldName + '</p>';
    // AReplaceText := ProcessorCompany.ContentFromString(AReplaceText);
  end;
end;

procedure TWebModule1.ProcessorGenericValue( Sender : TObject; const AObjectName, APropName : string;
  var AReplaceText : string; var AHandled : Boolean );
begin
  if SameText( AObjectName, 'dataE' ) and ( SameText( APropName, 'name' ) or SameText( APropName, 'value' ) )
  then
  begin
    AHandled := True;
    AReplaceText := 'You requested ' + APropName;
  end;
end;

procedure TWebModule1.ProcessorHomeValue( Sender : TObject; const AObjectName, APropName : string;
  var AReplaceText : string; var AHandled : Boolean );
begin
  if AObjectName = 'hello'
  then
    AReplaceText := 'Hello World';
end;

procedure TWebModule1.WebStencilsEngine1Language( Sender : TObject; const APropName : string;
  var AReplaceText : string );
begin
  if APropName = 'hellow'
  then
    AReplaceText := '¡Hola Mundo!';
end;

procedure TWebModule1.WebStencilsEngine1Value( Sender : TObject; const AObjectName, APropName : string;
  var AReplaceText : string; var AHandled : Boolean );
begin
  if SameText( AObjectName, 'dataE' ) and ( SameText( APropName, 'name' ) or SameText( APropName, 'value' ) )
  then
  begin
    AHandled := True;
    AReplaceText := 'You requested ' + APropName;
  end;
end;

procedure TWebModule1.WebModule1ActionCompanyAction( Sender : TObject;
  Request : TWebRequest; Response : TWebResponse; var Handled : Boolean );
begin
  var
  id := Request.QueryFields.Values[ 'id' ];
  ClientDataSet1.Open;
  if id <> ''
  then
    ClientDataSet1.Locate( 'CustNo', id, [ ] );
  ProcessorCompany.AddVar( 'dataset', ClientDataSet1, False ); // do not destroy
  Response.Content := ProcessorCompany.Content;
  ClientDataSet1.Close;
  Handled := True;
end;

procedure TWebModule1.WebModule1ActionGetValueFromEventAction( Sender : TObject; Request : TWebRequest;
  Response : TWebResponse; var Handled : Boolean );
begin
  ProcessorGeneric.InputFileName := '..\..\html\getvalueevt.html';
  Response.Content := ProcessorGeneric.Content;
  Handled := True;
end;

procedure TWebModule1.WebModule1ActionGetValueFromObjectAction( Sender : TObject; Request : TWebRequest;
  Response : TWebResponse; var Handled : Boolean );
begin
  ProcessorGeneric.InputFileName := '..\..\html\getvalueobj.html';
  ProcessorGeneric.AddVar( 'dataO', TSimpleObject.Create( 'joe', 55 ) );
  ProcessorGeneric.AddVar( 'obj1', TSimpleObject.Create( 'joe', 55 ) );
  ProcessorGeneric.AddVar( 'obj2', TSimpleObject.Create( 'marc', 9 ) );
  Response.Content := ProcessorGeneric.Content;
  Handled := True;
end;

procedure TWebModule1.WebModule1ActionIfAction( Sender : TObject;
  Request : TWebRequest; Response : TWebResponse; var Handled : Boolean );
begin
  ProcessorGeneric.InputFileName := '..\..\html\if.html';
  ProcessorGeneric.AddVar( 'obj1', TSimpleObject.Create( 'joe', 55 ) );
  ProcessorGeneric.AddVar( 'obj2', TSimpleObject.Create( 'marc', 9 ) );
  Response.Content := ProcessorGeneric.Content;
  Handled := True;
end;

procedure TWebModule1.WebModule1WebActionItem1Action( Sender : TObject;
  Request : TWebRequest; Response : TWebResponse; var Handled : Boolean );
begin
  ClientDataSet1.Open;
  ProcessorCompanyList.AddVar( 'dataset', ClientDataSet1, False ); // do not destroy
  Response.Content := ProcessorCompanyList.Content;
  ClientDataSet1.Close;
  Handled := True;
end;

end.
