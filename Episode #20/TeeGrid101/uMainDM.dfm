object MainDM: TMainDM
  Height = 750
  Width = 1000
  PixelsPerInch = 120
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 111
    Top = 51
  end
  object EmployeeTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM EMPLOYEE')
    Left = 111
    Top = 99
    object EmployeeTableEMP_NO: TSmallintField
      AutoGenerateValue = arAutoInc
      FieldName = 'EMP_NO'
      Origin = 'EMP_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object EmployeeTableFIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Origin = 'FIRST_NAME'
      Required = True
      Size = 15
    end
    object EmployeeTableLAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Origin = 'LAST_NAME'
      Required = True
    end
    object EmployeeTablePHONE_EXT: TStringField
      FieldName = 'PHONE_EXT'
      Origin = 'PHONE_EXT'
      Size = 4
    end
    object EmployeeTableHIRE_DATE: TSQLTimeStampField
      AutoGenerateValue = arDefault
      FieldName = 'HIRE_DATE'
      Origin = 'HIRE_DATE'
    end
    object EmployeeTableDEPT_NO: TStringField
      FieldName = 'DEPT_NO'
      Origin = 'DEPT_NO'
      Required = True
      FixedChar = True
      Size = 3
    end
    object EmployeeTableJOB_CODE: TStringField
      FieldName = 'JOB_CODE'
      Origin = 'JOB_CODE'
      Required = True
      Size = 5
    end
    object EmployeeTableJOB_GRADE: TSmallintField
      FieldName = 'JOB_GRADE'
      Origin = 'JOB_GRADE'
      Required = True
    end
    object EmployeeTableJOB_COUNTRY: TStringField
      FieldName = 'JOB_COUNTRY'
      Origin = 'JOB_COUNTRY'
      Required = True
      Size = 15
    end
    object EmployeeTableSALARY: TFMTBCDField
      FieldName = 'SALARY'
      Origin = 'SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
    object EmployeeTableFULL_NAME: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'FULL_NAME'
      Origin = 'FULL_NAME'
      ProviderFlags = []
      ReadOnly = True
      Size = 37
    end
  end
  object Salary_historyTable: TFDQuery
    IndexFieldNames = 'EMP_NO'
    MasterSource = DataSource1
    MasterFields = 'EMP_NO'
    DetailFields = 'EMP_NO'
    Connection = EmployeeConnection
    FetchOptions.AssignedValues = [evItems, evCache]
    SQL.Strings = (
      'SELECT * FROM SALARY_HISTORY'
      'where EMP_NO = :EMP_NO')
    Left = 112
    Top = 151
    ParamData = <
      item
        Name = 'EMP_NO'
        DataType = ftSmallint
        ParamType = ptInput
        Value = 2
      end>
    object Salary_historyTableEMP_NO: TSmallintField
      FieldName = 'EMP_NO'
      Origin = 'EMP_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object Salary_historyTableCHANGE_DATE: TSQLTimeStampField
      AutoGenerateValue = arDefault
      FieldName = 'CHANGE_DATE'
      Origin = 'CHANGE_DATE'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object Salary_historyTableUPDATER_ID: TStringField
      FieldName = 'UPDATER_ID'
      Origin = 'UPDATER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object Salary_historyTableOLD_SALARY: TFMTBCDField
      FieldName = 'OLD_SALARY'
      Origin = 'OLD_SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
    object Salary_historyTablePERCENT_CHANGE: TFloatField
      AutoGenerateValue = arDefault
      FieldName = 'PERCENT_CHANGE'
      Origin = 'PERCENT_CHANGE'
    end
    object Salary_historyTableNEW_SALARY: TFloatField
      AutoGenerateValue = arDefault
      FieldName = 'NEW_SALARY'
      Origin = 'NEW_SALARY'
      ProviderFlags = []
      ReadOnly = True
    end
  end
  object DataSource1: TDataSource
    DataSet = EmployeeTable
    Left = 224
    Top = 96
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 864
    Top = 48
  end
end
