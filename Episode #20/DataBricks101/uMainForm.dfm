object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Databricks 101'
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
    Tag = 23
    Left = 20
    Top = 59
    Width = 741
    Height = 461
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ColCount = 23
    DefaultColWidth = 155
    DefaultRowHeight = 30
    FixedCols = 0
    RowCount = 2
    TabOrder = 1
    ColWidths = (
      92
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      2044
      149
      276
      172
      276
      2044
      276)
    ColAligments = (
      1
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
      0
      0
      0
      0
      1
      0
      1
      0
      0
      0)
  end
  object RaddatabricksConnection: TFDConnection
    Params.Strings = (
      'Database='
      'ConnectionDef=RAD Databricks')
    Connected = True
    LoginPrompt = False
    Left = 137
    Top = 83
  end
  object FDQuery1: TFDQuery
    Connection = RaddatabricksConnection
    SQL.Strings = (
      'select * from nb_dev.load_address_nb')
    Left = 140
    Top = 150
    object FDQuery1address_id: TBCDField
      FieldName = 'address_id'
      Origin = 'address_id'
      Precision = 10
      Size = 0
    end
    object FDQuery1address_type_code: TStringField
      FieldName = 'address_type_code'
      Origin = 'address_type_code'
      Size = 255
    end
    object FDQuery1address_line_1: TStringField
      FieldName = 'address_line_1'
      Origin = 'address_line_1'
      Size = 255
    end
    object FDQuery1address_line_2: TStringField
      FieldName = 'address_line_2'
      Origin = 'address_line_2'
      Size = 255
    end
    object FDQuery1address_line_3: TStringField
      FieldName = 'address_line_3'
      Origin = 'address_line_3'
      Size = 255
    end
    object FDQuery1address_line_4: TStringField
      FieldName = 'address_line_4'
      Origin = 'address_line_4'
      Size = 255
    end
    object FDQuery1customer_code: TStringField
      FieldName = 'customer_code'
      Origin = 'customer_code'
      Size = 255
    end
    object FDQuery1city_code: TStringField
      FieldName = 'city_code'
      Origin = 'city_code'
      Size = 255
    end
    object FDQuery1city_name: TStringField
      FieldName = 'city_name'
      Origin = 'city_name'
      Size = 255
    end
    object FDQuery1state_code: TStringField
      FieldName = 'state_code'
      Origin = 'state_code'
      Size = 255
    end
    object FDQuery1zip_code: TStringField
      FieldName = 'zip_code'
      Origin = 'zip_code'
      Size = 255
    end
    object FDQuery1county_name: TStringField
      FieldName = 'county_name'
      Origin = 'county_name'
      Size = 255
    end
    object FDQuery1country_code: TStringField
      FieldName = 'country_code'
      Origin = 'country_code'
      Size = 255
    end
    object FDQuery1area_code: TStringField
      FieldName = 'area_code'
      Origin = 'area_code'
      Size = 255
    end
    object FDQuery1phone_number: TStringField
      FieldName = 'phone_number'
      Origin = 'phone_number'
      Size = 255
    end
    object FDQuery1primary_contact_person: TStringField
      FieldName = 'primary_contact_person'
      Origin = 'primary_contact_person'
      Size = 255
    end
    object FDQuery1secondary_contact_person: TStringField
      FieldName = 'secondary_contact_person'
      Origin = 'secondary_contact_person'
      Size = 255
    end
    object FDQuery1creating_employee_id: TBCDField
      FieldName = 'creating_employee_id'
      Origin = 'creating_employee_id'
      Precision = 10
      Size = 0
    end
    object FDQuery1created_datetime: TSQLTimeStampField
      FieldName = 'created_datetime'
      Origin = 'created_datetime'
    end
    object FDQuery1last_change_employee_id: TBCDField
      FieldName = 'last_change_employee_id'
      Origin = 'last_change_employee_id'
      Precision = 10
      Size = 0
    end
    object FDQuery1last_change_datetime: TSQLTimeStampField
      FieldName = 'last_change_datetime'
      Origin = 'last_change_datetime'
    end
    object FDQuery1dss_record_source: TStringField
      FieldName = 'dss_record_source'
      Origin = 'dss_record_source'
      Size = 255
    end
    object FDQuery1dss_load_date: TSQLTimeStampField
      FieldName = 'dss_load_date'
      Origin = 'dss_load_date'
    end
  end
  object BindSourceDB1: TBindSourceDB
    DataSet = FDQuery1
    ScopeMappings = <>
    Left = 140
    Top = 310
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 140
    Top = 225
    object LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = BindSourceDB1
      GridControl = StringGrid1
      Columns = <>
    end
  end
end
