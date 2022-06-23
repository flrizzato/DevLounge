unit uPoolingResource;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  EMS.DataSetResource, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Comp.UI, FireDAC.Phys.IBBase;

type
  [ResourceName('EmployeePooled')]
  TEmployeePooledResource1 = class(TDataModule)
    FDConnection1: TFDConnection;
    qryCUSTOMER: TFDQuery;
    [ResourceSuffix('CUSTOMER')]
    dsrCUSTOMER: TEMSDataSetResource;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    qryCUSTOMEREMP_NO: TSmallintField;
    qryCUSTOMERFIRST_NAME: TStringField;
    qryCUSTOMERLAST_NAME: TStringField;
    qryCUSTOMERPHONE_EXT: TStringField;
    qryCUSTOMERHIRE_DATE: TSQLTimeStampField;
    qryCUSTOMERDEPT_NO: TStringField;
    qryCUSTOMERJOB_CODE: TStringField;
    qryCUSTOMERJOB_GRADE: TSmallintField;
    qryCUSTOMERJOB_COUNTRY: TStringField;
    qryCUSTOMERSALARY: TFMTBCDField;
    qryCUSTOMERFULL_NAME: TStringField;
    procedure FDConnection1BeforeConnect(Sender: TObject);

  published
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure Register;
begin
  RegisterResource(TypeInfo(TEmployeePooledResource1));
end;

procedure SetupFDManager;
var
  oParams: TStrings;
begin
  oParams := TStringList.Create;
  oParams.Add('Protocol=TCPIP');
  oParams.Add('Database=192.168.1.200:C:\data\employee.gdb');
  oParams.Add('User_Name=firedac');
  oParams.Add('Password=123');
  oParams.Add('CharacterSet=');
  oParams.Add('ExtendedMetadata=True');
  oParams.Add('DriverID=IB');
  oParams.Add('Pooled=True');
  oParams.Add('POOL_MaximumItems=20');
  FDManager.AddConnectionDef('EMPLOYEE_POOLED', 'IB', oParams);
end;

procedure TEmployeePooledResource1.FDConnection1BeforeConnect(Sender: TObject);
begin
  FDConnection1.ConnectionDefName := 'EMPLOYEE_POOLED';
end;

initialization
  Register;
  SetupFDManager;
end.


