object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Pivot Demo'
  ClientHeight = 553
  ClientWidth = 1024
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 120
  TextHeight = 20
  object StringGrid1: TStringGrid
    Tag = 5
    AlignWithMargins = True
    Left = 5
    Top = 60
    Width = 1014
    Height = 488
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    DefaultColWidth = 80
    DefaultRowHeight = 40
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    ColWidths = (
      155
      355
      155
      155
      155)
    ColAligments = (
      1
      0
      0
      1
      1)
  end
  object Button1: TButton
    AlignWithMargins = True
    Left = 5
    Top = 10
    Width = 1014
    Height = 40
    Margins.Left = 5
    Margins.Top = 10
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Activate'
    TabOrder = 1
    OnClick = Button1Click
  end
  object FDSQLite: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 50
    Top = 50
  end
  object FDLocalSQL: TFDLocalSQL
    Connection = FDSQLite
    Active = True
    DataSets = <
      item
        DataSet = FDOrders
        Name = 'ORDERS'
      end
      item
        DataSet = FDCustomer
        Name = 'CUSTOMER'
      end>
    Left = 50
    Top = 130
  end
  object FDQueryPivot: TFDQuery
    LocalSQL = FDLocalSQL
    Connection = FDSQLite
    SQL.Strings = (
      'SELECT'
      '    O.CUSTNO,'
      '    C.COMPANY,'
      '    strftime('#39'%Y'#39', O.SALEDATE) AS SALE_YEAR,'
      '    SUM(O.ITEMSTOTAL) FILTER (WHERE O.SALEDATE IS NOT NULL)'
      
        '        OVER (PARTITION BY O.CUSTNO, strftime('#39'%Y'#39', O.SALEDATE))' +
        ' AS SALES_PER_YEAR,'
      
        '    SUM(O.ITEMSTOTAL) OVER (PARTITION BY O.CUSTNO) AS TOTAL_SALE' +
        'S'
      'FROM ORDERS O'
      'INNER JOIN CUSTOMER C ON (O.CUSTNO = C.CUSTNO)'
      'GROUP BY O.CUSTNO, SALE_YEAR'
      'ORDER BY O.CUSTNO, SALE_YEAR;'
      '')
    Left = 50
    Top = 210
    object FDQueryPivotCUSTNO: TFloatField
      FieldName = 'CUSTNO'
      Origin = 'CUSTNO'
    end
    object FDQueryPivotCOMPANY: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'COMPANY'
      Origin = 'COMPANY'
      ProviderFlags = []
      ReadOnly = True
      Size = 30
    end
    object FDQueryPivotSALE_YEAR: TWideStringField
      AutoGenerateValue = arDefault
      FieldName = 'SALE_YEAR'
      Origin = 'SALE_YEAR'
      ProviderFlags = []
      ReadOnly = True
      Size = 32767
    end
    object FDQueryPivotSALES_PER_YEAR: TFloatField
      AutoGenerateValue = arDefault
      FieldName = 'SALES_PER_YEAR'
      Origin = 'SALES_PER_YEAR'
      ProviderFlags = []
      ReadOnly = True
    end
    object FDQueryPivotTOTAL_SALES: TFloatField
      AutoGenerateValue = arDefault
      FieldName = 'TOTAL_SALES'
      Origin = 'TOTAL_SALES'
      ProviderFlags = []
      ReadOnly = True
    end
  end
  object FDInterbase: TFDConnection
    Params.Strings = (
      'Database=C:\data\MASTSQL.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'Port=3050'
      'DriverID=IB')
    LoginPrompt = False
    Left = 160
    Top = 50
  end
  object FDOrders: TFDQuery
    Connection = FDInterbase
    SQL.Strings = (
      'SELECT * FROM ORDERS ORDER BY ORDERNO')
    Left = 160
    Top = 130
    object FDOrdersORDERNO: TFloatField
      FieldName = 'ORDERNO'
      Origin = 'ORDERNO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object FDOrdersCUSTNO: TFloatField
      FieldName = 'CUSTNO'
      Origin = 'CUSTNO'
    end
    object FDOrdersSALEDATE: TSQLTimeStampField
      FieldName = 'SALEDATE'
      Origin = 'SALEDATE'
    end
    object FDOrdersSHIPDATE: TSQLTimeStampField
      FieldName = 'SHIPDATE'
      Origin = 'SHIPDATE'
    end
    object FDOrdersEMPNO: TIntegerField
      FieldName = 'EMPNO'
      Origin = 'EMPNO'
    end
    object FDOrdersSHIPTOCONTACT: TStringField
      FieldName = 'SHIPTOCONTACT'
      Origin = 'SHIPTOCONTACT'
    end
    object FDOrdersSHIPTOADDR1: TStringField
      FieldName = 'SHIPTOADDR1'
      Origin = 'SHIPTOADDR1'
      Size = 30
    end
    object FDOrdersSHIPTOADDR2: TStringField
      FieldName = 'SHIPTOADDR2'
      Origin = 'SHIPTOADDR2'
      Size = 30
    end
    object FDOrdersSHIPTOCITY: TStringField
      FieldName = 'SHIPTOCITY'
      Origin = 'SHIPTOCITY'
      Size = 15
    end
    object FDOrdersSHIPTOSTATE: TStringField
      FieldName = 'SHIPTOSTATE'
      Origin = 'SHIPTOSTATE'
    end
    object FDOrdersSHIPTOZIP: TStringField
      FieldName = 'SHIPTOZIP'
      Origin = 'SHIPTOZIP'
      Size = 10
    end
    object FDOrdersSHIPTOCOUNTRY: TStringField
      FieldName = 'SHIPTOCOUNTRY'
      Origin = 'SHIPTOCOUNTRY'
    end
    object FDOrdersSHIPTOPHONE: TStringField
      FieldName = 'SHIPTOPHONE'
      Origin = 'SHIPTOPHONE'
      Size = 15
    end
    object FDOrdersSHIPVIA: TStringField
      FieldName = 'SHIPVIA'
      Origin = 'SHIPVIA'
      Size = 7
    end
    object FDOrdersPO: TStringField
      FieldName = 'PO'
      Origin = 'PO'
      Size = 15
    end
    object FDOrdersTERMS: TStringField
      FieldName = 'TERMS'
      Origin = 'TERMS'
      Size = 6
    end
    object FDOrdersPAYMENTMETHOD: TStringField
      FieldName = 'PAYMENTMETHOD'
      Origin = 'PAYMENTMETHOD'
      Size = 7
    end
    object FDOrdersITEMSTOTAL: TFloatField
      FieldName = 'ITEMSTOTAL'
      Origin = 'ITEMSTOTAL'
    end
    object FDOrdersTAXRATE: TFloatField
      FieldName = 'TAXRATE'
      Origin = 'TAXRATE'
    end
    object FDOrdersFREIGHT: TFloatField
      FieldName = 'FREIGHT'
      Origin = 'FREIGHT'
    end
    object FDOrdersAMOUNTPAID: TFloatField
      FieldName = 'AMOUNTPAID'
      Origin = 'AMOUNTPAID'
    end
  end
  object BindSourceDB1: TBindSourceDB
    DataSet = FDOrders
    ScopeMappings = <>
    Left = 270
    Top = 350
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 270
    Top = 275
    object LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = BindSourceDB2
      GridControl = StringGrid1
      Columns = <
        item
          MemberName = 'CUSTNO'
          Width = 155
        end
        item
          MemberName = 'COMPANY'
          Width = 355
        end
        item
          MemberName = 'SALE_YEAR'
          Width = 155
        end
        item
          MemberName = 'SALES_PER_YEAR'
          Width = 155
        end
        item
          MemberName = 'TOTAL_SALES'
          Width = 155
        end>
    end
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 650
    Top = 440
  end
  object FDCustomer: TFDQuery
    Connection = FDInterbase
    SQL.Strings = (
      'SELECT * FROM CUSTOMER ORDER BY CUSTNO')
    Left = 255
    Top = 133
    object FDCustomerCUSTNO: TFloatField
      FieldName = 'CUSTNO'
      Origin = 'CUSTNO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object FDCustomerCOMPANY: TStringField
      FieldName = 'COMPANY'
      Origin = 'COMPANY'
      Required = True
      Size = 30
    end
    object FDCustomerADDR1: TStringField
      FieldName = 'ADDR1'
      Origin = 'ADDR1'
      Size = 30
    end
    object FDCustomerADDR2: TStringField
      FieldName = 'ADDR2'
      Origin = 'ADDR2'
      Size = 30
    end
    object FDCustomerCITY: TStringField
      FieldName = 'CITY'
      Origin = 'CITY'
      Size = 15
    end
    object FDCustomerSTATE: TStringField
      FieldName = 'STATE'
      Origin = 'STATE'
    end
    object FDCustomerZIP: TStringField
      FieldName = 'ZIP'
      Origin = 'ZIP'
      Size = 10
    end
    object FDCustomerCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
    end
    object FDCustomerPHONE: TStringField
      FieldName = 'PHONE'
      Origin = 'PHONE'
      Size = 15
    end
    object FDCustomerFAX: TStringField
      FieldName = 'FAX'
      Origin = 'FAX'
      Size = 15
    end
    object FDCustomerTAXRATE: TFloatField
      FieldName = 'TAXRATE'
      Origin = 'TAXRATE'
    end
    object FDCustomerCONTACT: TStringField
      FieldName = 'CONTACT'
      Origin = 'CONTACT'
    end
    object FDCustomerLASTINVOICEDATE: TSQLTimeStampField
      FieldName = 'LASTINVOICEDATE'
      Origin = 'LASTINVOICEDATE'
    end
  end
  object BindSourceDB2: TBindSourceDB
    DataSet = FDQueryPivot
    ScopeMappings = <>
    Left = 380
    Top = 280
  end
end
