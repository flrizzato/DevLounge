object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Oracle Autonomous 101'
  ClientHeight = 553
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 120
  TextHeight = 20
  object Button1: TButton
    Left = 20
    Top = 20
    Width = 151
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Connect'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Tag = 23
    Left = 20
    Top = 70
    Width = 741
    Height = 451
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ColCount = 23
    DefaultColWidth = 80
    DefaultRowHeight = 30
    FixedCols = 0
    RowCount = 2
    GridLineWidth = 0
    TabOrder = 1
    ColWidths = (
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155
      155)
    ColAligments = (
      1
      0
      0
      0
      1
      0
      0
      0
      0
      1
      0
      1
      1
      0
      0
      1
      0
      0
      1
      1
      0
      0
      0)
  end
  object T1ixblbn7708yv6x_tpConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=t1ixblbn7708yv6x_tp')
    FetchOptions.AssignedValues = [evLiveWindowParanoic]
    FetchOptions.LiveWindowParanoic = False
    LoginPrompt = False
    Left = 145
    Top = 150
  end
  object CustomersTable: TFDQuery
    Connection = T1ixblbn7708yv6x_tpConnection
    FetchOptions.AssignedValues = [evAutoFetchAll, evLiveWindowParanoic]
    FetchOptions.LiveWindowParanoic = False
    SQL.Strings = (
      'SELECT * FROM SH.CUSTOMERS '
      'ORDER BY CUST_ID '
      'FETCH FIRST 50 ROWS ONLY')
    Left = 145
    Top = 220
    object CustomersTableCUST_ID: TFMTBCDField
      FieldName = 'CUST_ID'
      Origin = 'CUST_ID'
      Required = True
      Precision = 38
      Size = 38
    end
    object CustomersTableCUST_FIRST_NAME: TStringField
      FieldName = 'CUST_FIRST_NAME'
      Origin = 'CUST_FIRST_NAME'
      Required = True
    end
    object CustomersTableCUST_LAST_NAME: TStringField
      FieldName = 'CUST_LAST_NAME'
      Origin = 'CUST_LAST_NAME'
      Required = True
      Size = 40
    end
    object CustomersTableCUST_GENDER: TStringField
      FieldName = 'CUST_GENDER'
      Origin = 'CUST_GENDER'
      Required = True
      FixedChar = True
      Size = 1
    end
    object CustomersTableCUST_YEAR_OF_BIRTH: TBCDField
      FieldName = 'CUST_YEAR_OF_BIRTH'
      Origin = 'CUST_YEAR_OF_BIRTH'
      Required = True
      Precision = 4
      Size = 0
    end
    object CustomersTableCUST_MARITAL_STATUS: TStringField
      FieldName = 'CUST_MARITAL_STATUS'
      Origin = 'CUST_MARITAL_STATUS'
    end
    object CustomersTableCUST_STREET_ADDRESS: TStringField
      FieldName = 'CUST_STREET_ADDRESS'
      Origin = 'CUST_STREET_ADDRESS'
      Required = True
      Size = 40
    end
    object CustomersTableCUST_POSTAL_CODE: TStringField
      FieldName = 'CUST_POSTAL_CODE'
      Origin = 'CUST_POSTAL_CODE'
      Required = True
      Size = 10
    end
    object CustomersTableCUST_CITY: TStringField
      FieldName = 'CUST_CITY'
      Origin = 'CUST_CITY'
      Required = True
      Size = 30
    end
    object CustomersTableCUST_CITY_ID: TFMTBCDField
      FieldName = 'CUST_CITY_ID'
      Origin = 'CUST_CITY_ID'
      Required = True
      Precision = 38
      Size = 38
    end
    object CustomersTableCUST_STATE_PROVINCE: TStringField
      FieldName = 'CUST_STATE_PROVINCE'
      Origin = 'CUST_STATE_PROVINCE'
      Required = True
      Size = 40
    end
    object CustomersTableCUST_STATE_PROVINCE_ID: TFMTBCDField
      FieldName = 'CUST_STATE_PROVINCE_ID'
      Origin = 'CUST_STATE_PROVINCE_ID'
      Required = True
      Precision = 38
      Size = 38
    end
    object CustomersTableCOUNTRY_ID: TFMTBCDField
      FieldName = 'COUNTRY_ID'
      Origin = 'COUNTRY_ID'
      Required = True
      Precision = 38
      Size = 38
    end
    object CustomersTableCUST_MAIN_PHONE_NUMBER: TStringField
      FieldName = 'CUST_MAIN_PHONE_NUMBER'
      Origin = 'CUST_MAIN_PHONE_NUMBER'
      Required = True
      Size = 25
    end
    object CustomersTableCUST_INCOME_LEVEL: TStringField
      FieldName = 'CUST_INCOME_LEVEL'
      Origin = 'CUST_INCOME_LEVEL'
      Size = 30
    end
    object CustomersTableCUST_CREDIT_LIMIT: TFMTBCDField
      FieldName = 'CUST_CREDIT_LIMIT'
      Origin = 'CUST_CREDIT_LIMIT'
      Precision = 38
      Size = 38
    end
    object CustomersTableCUST_EMAIL: TStringField
      FieldName = 'CUST_EMAIL'
      Origin = 'CUST_EMAIL'
      Size = 50
    end
    object CustomersTableCUST_TOTAL: TStringField
      FieldName = 'CUST_TOTAL'
      Origin = 'CUST_TOTAL'
      Required = True
      Size = 14
    end
    object CustomersTableCUST_TOTAL_ID: TFMTBCDField
      FieldName = 'CUST_TOTAL_ID'
      Origin = 'CUST_TOTAL_ID'
      Required = True
      Precision = 38
      Size = 38
    end
    object CustomersTableCUST_SRC_ID: TFMTBCDField
      FieldName = 'CUST_SRC_ID'
      Origin = 'CUST_SRC_ID'
      Precision = 38
      Size = 38
    end
    object CustomersTableCUST_EFF_FROM: TDateTimeField
      FieldName = 'CUST_EFF_FROM'
      Origin = 'CUST_EFF_FROM'
    end
    object CustomersTableCUST_EFF_TO: TDateTimeField
      FieldName = 'CUST_EFF_TO'
      Origin = 'CUST_EFF_TO'
    end
    object CustomersTableCUST_VALID: TStringField
      FieldName = 'CUST_VALID'
      Origin = 'CUST_VALID'
      Size = 1
    end
  end
  object BindSourceDB1: TBindSourceDB
    DataSet = CustomersTable
    ScopeMappings = <>
    Left = 146
    Top = 385
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 146
    Top = 301
    object LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = BindSourceDB1
      GridControl = StringGrid1
      Columns = <
        item
          MemberName = 'CUST_ID'
          Width = 155
        end
        item
          MemberName = 'CUST_FIRST_NAME'
          Width = 155
        end
        item
          MemberName = 'CUST_LAST_NAME'
          Width = 155
        end
        item
          MemberName = 'CUST_GENDER'
          Width = 155
        end
        item
          MemberName = 'CUST_YEAR_OF_BIRTH'
          Width = 155
        end
        item
          MemberName = 'CUST_MARITAL_STATUS'
          Width = 155
        end
        item
          MemberName = 'CUST_STREET_ADDRESS'
          Width = 155
        end
        item
          MemberName = 'CUST_POSTAL_CODE'
          Width = 155
        end
        item
          MemberName = 'CUST_CITY'
          Width = 155
        end
        item
          MemberName = 'CUST_CITY_ID'
          Width = 155
        end
        item
          MemberName = 'CUST_STATE_PROVINCE'
          Width = 155
        end
        item
          MemberName = 'CUST_STATE_PROVINCE_ID'
          Width = 155
        end
        item
          MemberName = 'COUNTRY_ID'
          Width = 155
        end
        item
          MemberName = 'CUST_MAIN_PHONE_NUMBER'
          Width = 155
        end
        item
          MemberName = 'CUST_INCOME_LEVEL'
          Width = 155
        end
        item
          MemberName = 'CUST_CREDIT_LIMIT'
          Width = 155
        end
        item
          MemberName = 'CUST_EMAIL'
          Width = 155
        end
        item
          MemberName = 'CUST_TOTAL'
          Width = 155
        end
        item
          MemberName = 'CUST_TOTAL_ID'
          Width = 155
        end
        item
          MemberName = 'CUST_SRC_ID'
          Width = 155
        end
        item
          MemberName = 'CUST_EFF_FROM'
          Width = 155
        end
        item
          MemberName = 'CUST_EFF_TO'
          Width = 155
        end
        item
          MemberName = 'CUST_VALID'
          Width = 155
        end>
    end
  end
end
