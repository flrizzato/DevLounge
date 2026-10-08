object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 45
    Width = 34
    Height = 15
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 513
    Height = 23
    TabOrder = 0
    Text = 'Edit1'
    OnKeyDown = Edit1KeyDown
  end
  object Button1: TButton
    Left = 527
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    Enabled = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 112
    Width = 624
    Height = 329
    Align = alBottom
    ColCount = 1
    FixedCols = 0
    RowCount = 2
    TabOrder = 2
    ColWidths = (
      64)
    ColAligments = (
      1)
  end
  object Button2: TButton
    Left = 549
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\Public\Documents\Embarcadero\Studio\37.0\Sampl' +
        'es\Data\Employees.s3db'
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 240
    Top = 56
  end
  object BindSourceDB1: TBindSourceDB
    ScopeMappings = <>
    Left = 152
    Top = 64
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 68
    Top = 61
    object LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = BindSourceDB2
      GridControl = StringGrid1
      Columns = <>
    end
  end
  object AIConnection1: TAIConnection
    Driver = AIOllamaDriver
    Left = 560
    Top = 56
  end
  object AIOllamaDriver: TAIOllamaDriver
    Params.Strings = (
      'Model=celia:latest')
    Left = 440
    Top = 176
  end
  object AIChatRequest1: TAIChatRequest
    Connection = AIConnection1
    OnResponse = AIChatRequest1Response
    Left = 560
    Top = 168
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from Employee')
    Left = 368
    Top = 56
  end
  object BindSourceDB2: TBindSourceDB
    DataSet = FDQuery1
    ScopeMappings = <>
    Left = 304
    Top = 224
  end
end
