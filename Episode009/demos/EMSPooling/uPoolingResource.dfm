object EmployeePooledResource1: TEmployeePooledResource1
  Height = 300
  Width = 600
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    BeforeConnect = FDConnection1BeforeConnect
    Left = 80
    Top = 16
  end
  object qryCUSTOMER: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from EMP_LIST')
    Left = 192
    Top = 24
    object qryCUSTOMEREMP_NO: TSmallintField
      FieldName = 'EMP_NO'
      Origin = 'EMP_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object qryCUSTOMERFIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Origin = 'FIRST_NAME'
      Size = 15
    end
    object qryCUSTOMERLAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Origin = 'LAST_NAME'
    end
    object qryCUSTOMERPHONE_EXT: TStringField
      FieldName = 'PHONE_EXT'
      Origin = 'PHONE_EXT'
      Size = 4
    end
    object qryCUSTOMERHIRE_DATE: TSQLTimeStampField
      FieldName = 'HIRE_DATE'
      Origin = 'HIRE_DATE'
    end
    object qryCUSTOMERDEPT_NO: TStringField
      FieldName = 'DEPT_NO'
      Origin = 'DEPT_NO'
      FixedChar = True
      Size = 3
    end
    object qryCUSTOMERJOB_CODE: TStringField
      FieldName = 'JOB_CODE'
      Origin = 'JOB_CODE'
      Size = 5
    end
    object qryCUSTOMERJOB_GRADE: TSmallintField
      FieldName = 'JOB_GRADE'
      Origin = 'JOB_GRADE'
    end
    object qryCUSTOMERJOB_COUNTRY: TStringField
      FieldName = 'JOB_COUNTRY'
      Origin = 'JOB_COUNTRY'
      Size = 15
    end
    object qryCUSTOMERSALARY: TFMTBCDField
      FieldName = 'SALARY'
      Origin = 'SALARY'
      Precision = 18
      Size = 2
    end
    object qryCUSTOMERFULL_NAME: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'FULL_NAME'
      Origin = 'FULL_NAME'
      ProviderFlags = []
      ReadOnly = True
      Size = 37
    end
  end
  object dsrCUSTOMER: TEMSDataSetResource
    AllowedActions = [List, Get, Post, Put, Delete]
    DataSet = qryCUSTOMER
    KeyFields = 'EMP_NO'
    Left = 192
    Top = 80
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 80
    Top = 80
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    ScreenCursor = gcrHourGlass
    Left = 80
    Top = 144
  end
end
