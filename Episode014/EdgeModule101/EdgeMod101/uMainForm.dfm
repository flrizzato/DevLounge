object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'Edge Module 101'
  ClientHeight = 450
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 120
  TextHeight = 20
  object Button1: TButton
    Left = 30
    Top = 30
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object EMSProvider1: TEMSProvider
    ApiVersion = '2'
    URLHost = 'localhost'
    URLPort = 8080
    Left = 80
    Top = 100
  end
  object EMSEdgeService1: TEMSEdgeService
    AutoActivate = False
    ModuleName = 'location'
    ModuleVersion = '1'
    Provider = EMSProvider1
    ListenerProtocol = 'http'
    ListenerService.Port = 9090
    ListenerService.Host = 'localhost'
    Left = 100
    Top = 208
  end
end
