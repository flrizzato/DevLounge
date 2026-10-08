object WebModule1: TWebModule1
  Actions = <
    item
      Enabled = False
      Name = 'DefaultHandler'
      PathInfo = '/employeelist'
      OnAction = WebModule1DefaultHandlerAction
    end
    item
      Default = True
      Name = 'ActionEmployeeList'
      PathInfo = '/employeelist'
      OnAction = WebModule1ActionEmployeeListAction
    end
    item
      Name = 'ActionEmployeeUpdate'
      PathInfo = '/employeeupdate'
      OnAction = WebModule1ActionEmployeeEditAction
    end
    item
      Name = 'ActionEmployeeApply'
      PathInfo = '/employeeapply'
      OnAction = WebModule1ActionEmployeeApplyAction
    end>
  Height = 362
  Width = 623
  PixelsPerInch = 120
  object WebStencilsEngine1: TWebStencilsEngine
    PathTemplates = <
      item
        Template = '/{filename}'
      end>
    RootDirectory = '..\..\html\'
    Left = 96
    Top = 32
  end
  object WebStencilsProcessor1: TWebStencilsProcessor
    Engine = WebStencilsEngine1
    InputFileName = '..\..\html\employeelist.html'
    Left = 96
    Top = 120
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 267
    Top = 32
  end
  object EmployeeTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM EMPLOYEE ORDER BY EMP_NO')
    Left = 259
    Top = 120
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
  object WebStencilsProcessor2: TWebStencilsProcessor
    Engine = WebStencilsEngine1
    InputFileName = '..\..\html\employeeupdate.html'
    Left = 96
    Top = 208
  end
  object EmployeeUpdate: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM EMPLOYEE'
      ' WHERE EMP_NO = :EMP_NO  ')
    Left = 259
    Top = 208
    ParamData = <
      item
        Name = 'EMP_NO'
        DataType = ftSmallint
        ParamType = ptInput
      end>
    object EmployeeUpdateEMP_NO: TSmallintField
      AutoGenerateValue = arAutoInc
      FieldName = 'EMP_NO'
      Origin = 'EMP_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object EmployeeUpdateFIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Origin = 'FIRST_NAME'
      Required = True
      Size = 15
    end
    object EmployeeUpdateLAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Origin = 'LAST_NAME'
      Required = True
    end
    object EmployeeUpdatePHONE_EXT: TStringField
      FieldName = 'PHONE_EXT'
      Origin = 'PHONE_EXT'
      Size = 4
    end
    object EmployeeUpdateHIRE_DATE: TSQLTimeStampField
      AutoGenerateValue = arDefault
      FieldName = 'HIRE_DATE'
      Origin = 'HIRE_DATE'
    end
    object EmployeeUpdateDEPT_NO: TStringField
      FieldName = 'DEPT_NO'
      Origin = 'DEPT_NO'
      Required = True
      FixedChar = True
      Size = 3
    end
    object EmployeeUpdateJOB_CODE: TStringField
      FieldName = 'JOB_CODE'
      Origin = 'JOB_CODE'
      Required = True
      Size = 5
    end
    object EmployeeUpdateJOB_GRADE: TSmallintField
      FieldName = 'JOB_GRADE'
      Origin = 'JOB_GRADE'
      Required = True
    end
    object EmployeeUpdateJOB_COUNTRY: TStringField
      FieldName = 'JOB_COUNTRY'
      Origin = 'JOB_COUNTRY'
      Required = True
      Size = 15
    end
    object EmployeeUpdateSALARY: TFMTBCDField
      FieldName = 'SALARY'
      Origin = 'SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
    object EmployeeUpdateFULL_NAME: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'FULL_NAME'
      Origin = 'FULL_NAME'
      ProviderFlags = []
      ReadOnly = True
      Size = 37
    end
  end
end
