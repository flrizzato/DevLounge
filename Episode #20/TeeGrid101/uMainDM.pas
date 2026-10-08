unit uMainDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Stan.StorageBin;

type
  TMainDM = class(TDataModule)
    EmployeeConnection: TFDConnection;
    EmployeeTable: TFDQuery;
    Salary_historyTable: TFDQuery;
    DataSource1: TDataSource;
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
    Salary_historyTableEMP_NO: TSmallintField;
    Salary_historyTableCHANGE_DATE: TSQLTimeStampField;
    Salary_historyTableUPDATER_ID: TStringField;
    Salary_historyTableOLD_SALARY: TFMTBCDField;
    Salary_historyTablePERCENT_CHANGE: TFloatField;
    Salary_historyTableNEW_SALARY: TFloatField;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
  private
    { Private declarations }
    function CloneData(const ADataSet:TFDDataSet):TDataSet;
  public
    { Public declarations }
    function SalaryOfEmployee(const ARecNo:Integer):TDataSet;
  end;

var
  MainDM: TMainDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TMainDM }

function TMainDM.CloneData(const ADataSet: TFDDataSet): TDataSet;
var tmp : TMemoryStream;
begin
  tmp:=TMemoryStream.Create;
  try
    ADataSet.SaveToStream(tmp);
    tmp.Position:=0;

    result:=TFDMemTable.Create(nil);
    TFDMemTable(result).LoadFromStream(tmp);
  finally
    tmp.Free;
  end;end;

function TMainDM.SalaryOfEmployee(const ARecNo: Integer): TDataSet;
  // Return the CustomerID for row: ARow
  function EmployeeID:String;
  begin
    EmployeeTable.RecNo:=ARecNo;
    result:=EmployeeTable.FieldByName('Emp_No').AsString;
  end;

  // Execute OrdersTable query for a given CustomerID
  procedure FilterSalary(const AEmp_No:String);
  begin
    Salary_historyTable.Close;
    Salary_historyTable.ParamByName('Emp_no').AsString := AEmp_No;
    Salary_historyTable.Open;
  end;

begin
  FilterSalary(EmployeeID);
  result:=CloneData(Salary_historyTable);
end;

end.
