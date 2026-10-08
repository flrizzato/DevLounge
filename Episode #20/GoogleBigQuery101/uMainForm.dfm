object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Google Big Query 101'
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
    Width = 171
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
    Tag = 20
    Left = 20
    Top = 59
    Width = 741
    Height = 461
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ColCount = 20
    DefaultColWidth = 155
    DefaultRowHeight = 30
    FixedCols = 0
    RowCount = 2
    TabOrder = 1
    ColWidths = (
      131076
      131076
      131076
      131076
      131076
      131076
      131076
      131076
      131076
      84
      84
      84
      124
      84
      84
      84
      84
      124
      131076
      84)
    ColAligments = (
      0
      0
      0
      0
      0
      0
      0
      0
      0
      0
      0
      0
      1
      1
      1
      1
      1
      1
      0
      1)
  end
  object RadbigqueryConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=RAD BigQuery')
    Connected = True
    LoginPrompt = False
    Left = 122
    Top = 49
  end
  object FDQuery1: TFDQuery
    Connection = RadbigqueryConnection
    SQL.Strings = (
      
        'SELECT * FROM top-caldron-579.bistudio_example.bistudio LIMIT 10' +
        '00')
    Left = 120
    Top = 130
    object FDQuery1SalesReason: TStringField
      FieldName = 'Sales Reason'
      Origin = '`Sales Reason`'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1CustomerName: TStringField
      FieldName = 'Customer Name'
      Origin = '`Customer Name`'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1Territory: TStringField
      FieldName = 'Territory'
      Origin = 'Territory'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1ShipMethod: TStringField
      FieldName = 'Ship Method'
      Origin = '`Ship Method`'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1CurrencyCode: TStringField
      FieldName = 'Currency Code'
      Origin = '`Currency Code`'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1CardType: TStringField
      FieldName = 'Card Type'
      Origin = '`Card Type`'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1City: TStringField
      FieldName = 'City'
      Origin = 'City'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1State: TStringField
      FieldName = 'State'
      Origin = 'State'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1PostalCode: TStringField
      FieldName = 'PostalCode'
      Origin = 'PostalCode'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1OrderDate: TDateField
      FieldName = 'OrderDate'
      Origin = 'OrderDate'
      ReadOnly = True
    end
    object FDQuery1DueDate: TDateField
      FieldName = 'DueDate'
      Origin = 'DueDate'
      ReadOnly = True
    end
    object FDQuery1ShipDate: TDateField
      FieldName = 'ShipDate'
      Origin = 'ShipDate'
      ReadOnly = True
    end
    object FDQuery1OnlineOrderFlag: TLargeintField
      FieldName = 'Online Order Flag'
      Origin = '`Online Order Flag`'
      ReadOnly = True
    end
    object FDQuery1SubTotal: TFloatField
      FieldName = 'Sub Total'
      Origin = '`Sub Total`'
      ReadOnly = True
    end
    object FDQuery1TaxAmount: TFloatField
      FieldName = 'Tax Amount'
      Origin = '`Tax Amount`'
      ReadOnly = True
    end
    object FDQuery1Freight: TFloatField
      FieldName = 'Freight'
      Origin = 'Freight'
      ReadOnly = True
    end
    object FDQuery1TotalDue: TFloatField
      FieldName = 'Total Due'
      Origin = '`Total Due`'
      ReadOnly = True
    end
    object FDQuery1SalesOrderID: TLargeintField
      FieldName = 'Sales Order ID'
      Origin = '`Sales Order ID`'
      ReadOnly = True
    end
    object FDQuery1ProductCategory: TStringField
      FieldName = 'Product Category'
      Origin = '`Product Category`'
      ReadOnly = True
      Size = 16384
    end
    object FDQuery1Profit: TFloatField
      FieldName = 'Profit'
      Origin = 'Profit'
      ReadOnly = True
    end
  end
  object BindSourceDB1: TBindSourceDB
    DataSet = FDQuery1
    ScopeMappings = <>
    Left = 130
    Top = 220
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 130
    Top = 305
    object LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = BindSourceDB1
      GridControl = StringGrid1
      Columns = <>
    end
  end
end
