object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Teradata 101'
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
    Left = 20
    Top = 59
    Width = 741
    Height = 461
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ColCount = 1
    DefaultColWidth = 155
    DefaultRowHeight = 30
    FixedCols = 0
    RowCount = 2
    TabOrder = 1
    ColWidths = (
      155)
    ColAligments = (
      0)
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'User_Name=demo_user'
      'Password=DataBase123#'
      'Server=radteradata-nxawm67zx3kl493y.env.clearscape.teradata.com'
      'MonitorBy=FlatFile'
      'DriverID=TData')
    LoginPrompt = False
    Left = 219
    Top = 25
  end
  object FDMoniFlatFileClientLink1: TFDMoniFlatFileClientLink
    FileName = '.\trace.txt'
    Left = 226
    Top = 193
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT TOP 50 * FROM demo_user.bistudio_example')
    Left = 219
    Top = 103
  end
  object BindSourceDB1: TBindSourceDB
    DataSet = FDQuery1
    ScopeMappings = <>
    Left = 216
    Top = 285
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 206
    Top = 373
    object LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = BindSourceDB1
      GridControl = StringGrid1
      Columns = <>
    end
  end
end
