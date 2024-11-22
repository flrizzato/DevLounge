unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, Web.Stencils, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TWebModule1 = class(TWebModule)
    WebStencilsEngine1: TWebStencilsEngine;
    WebStencilsProcessor1: TWebStencilsProcessor;
    EmployeeConnection: TFDConnection;
    EmployeeTable: TFDQuery;
    EmployeeTableEMP_NO: TSmallintField;
    EmployeeTableFIRST_NAME: TStringField;
    EmployeeTableLAST_NAME: TStringField;
    EmployeeTablePHONE_EXT: TStringField;
    EmployeeTableHIRE_DATE: TSQLTimeStampField;
    EmployeeTableDEPT_NO: TStringField;
    EmployeeTableJOB_CODE: TStringField;
    EmployeeTableJOB_GRADE: TSmallintField;
    EmployeeTableJOB_COUNTRY: TStringField;
    EmployeeTableSALARY: TFMTBCDField;
    EmployeeTableFULL_NAME: TStringField;
    WebStencilsProcessor2: TWebStencilsProcessor;
    EmployeeUpdate: TFDQuery;
    EmployeeUpdateEMP_NO: TSmallintField;
    EmployeeUpdateFIRST_NAME: TStringField;
    EmployeeUpdateLAST_NAME: TStringField;
    EmployeeUpdatePHONE_EXT: TStringField;
    EmployeeUpdateHIRE_DATE: TSQLTimeStampField;
    EmployeeUpdateDEPT_NO: TStringField;
    EmployeeUpdateJOB_CODE: TStringField;
    EmployeeUpdateJOB_GRADE: TSmallintField;
    EmployeeUpdateJOB_COUNTRY: TStringField;
    EmployeeUpdateSALARY: TFMTBCDField;
    EmployeeUpdateFULL_NAME: TStringField;
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1ActionEmployeeListAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1ActionEmployeeEditAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1ActionEmployeeApplyAction(Sender: TObject;
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

procedure TWebModule1.WebModule1ActionEmployeeApplyAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    var
    lId := Request.QueryFields.Values['id'];
    EmployeeUpdate.Params[0].Value := lId;
    EmployeeUpdate.Open;

    EmployeeConnection.StartTransaction;
    try
      EmployeeUpdate.Edit;
      EmployeeUpdateFIRST_NAME.AsString := Request.ContentFields.Values['FIRST_NAME'];
      EmployeeUpdateLAST_NAME.AsString := Request.ContentFields.Values['LAST_NAME'];
      EmployeeUpdatePHONE_EXT.AsString := Request.ContentFields.Values['PHONE_EXT'];
      EmployeeUpdateHIRE_DATE.AsString := Request.ContentFields.Values['HIRE_date'];
      EmployeeUpdateSALARY.AsString := Request.ContentFields.Values['SALARY'];
      EmployeeUpdate.Post;
      EmployeeConnection.Commit;
      Response.SetCustomHeader('hx-redirect', '/employeelist');
    except
      on E: Exception do
        Response.SetCustomHeader('hx-redirect', '/someerrorpage');
    end;
  finally
    EmployeeUpdate.Close;
    Handled := True;
  end;
end;

procedure TWebModule1.WebModule1ActionEmployeeEditAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var lId := Request.QueryFields.Values['id'];
  EmployeeUpdate.Params[0].Value := lId;
  EmployeeUpdate.Open;
  WebStencilsProcessor2.AddVar('emp', EmployeeUpdate, False );
  Response.Content := WebStencilsProcessor2.Content;
  EmployeeUpdate.Close;
  Handled := True;
end;

procedure TWebModule1.WebModule1ActionEmployeeListAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  EmployeeTable.Open;
  WebStencilsProcessor1.AddVar('dataset', EmployeeTable, False );
  Response.Content := WebStencilsProcessor1.Content;
  EmployeeTable.Close;
  Handled := True;
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

end.
