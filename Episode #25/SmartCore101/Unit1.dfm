object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Form1'
  ClientHeight = 553
  ClientWidth = 748
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 20
  object Edit1: TEdit
    Left = 30
    Top = 30
    Width = 601
    Height = 28
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
    Text = 'Edit1'
  end
  object Memo1: TMemo
    Left = 30
    Top = 80
    Width = 681
    Height = 431
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 1
  end
  object Button1: TButton
    Left = 639
    Top = 29
    Width = 94
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object AIConnection1: TAIConnection
    Driver = AIOpenAIDriver1
    Left = 100
    Top = 130
  end
  object AIChatRequest1: TAIChatRequest
    Connection = AIConnection1
    Left = 100
    Top = 300
  end
  object AIChatBindSource1: TAIChatBindSource
    AutoActivate = True
    ScopeMappings = <>
    Left = 240
    Top = 220
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 240
    Top = 295
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = AIChatBindSource1
      FieldName = 'Text'
      Control = Memo1
      Track = False
    end
  end
  object AIOpenAIDriver1: TAIOpenAIDriver
    Left = 100
    Top = 220
  end
end
