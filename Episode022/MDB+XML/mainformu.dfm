object MainForm: TMainForm
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'MDB+XML'
  ClientHeight = 309
  ClientWidth = 830
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OnShow = FormShow
  PixelsPerInch = 120
  DesignSize = (
    830
    309)
  TextHeight = 17
  object DBGrid1: TDBGrid
    Left = 10
    Top = 49
    Width = 809
    Height = 250
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -14
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Employee'
        Width = 194
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OrderID'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CustomerID'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'EmployeeID'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OrderDate'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'RequiredDate'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShippedDate'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipVia'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Freight'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipName'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipAddress'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipCity'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipRegion'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipPostalCode'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipCountry'
        Width = 80
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 3
    Top = 10
    Width = 300
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DataSource = DataSource1
    TabOrder = 1
  end
  object Button1: TButton
    Left = 710
    Top = 10
    Width = 110
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MS Access'
    TabOrder = 2
    OnClick = Button1Click
  end
  object DataSource1: TDataSource
    DataSet = MainDM.FDQuery1
    Left = 60
    Top = 80
  end
end
