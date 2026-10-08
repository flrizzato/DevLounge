object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'MongoDB Demo 101'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 782
    Height = 553
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'sample_analytics'
      object Button1: TButton
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 764
        Height = 31
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        Caption = 'Open >>'
        TabOrder = 0
        OnClick = Button1Click
      end
      object DBGrid1: TDBGrid
        AlignWithMargins = True
        Left = 5
        Top = 46
        Width = 764
        Height = 467
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        DataSource = DataSource1
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -15
        TitleFont.Name = 'Segoe UI'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = '_id'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'username'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'name'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'address'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'birthdate'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'email'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'active'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'accounts'
            Visible = True
          end>
      end
    end
  end
  object MongoDBConnection: TFDConnection
    Params.Strings = (
      'User_Name=flrizzato'
      'Password=PDzo0QaynJd9cR7S'
      'UseSRV=True'
      'Server=radmongodb.brkpz.mongodb.net'
      'DriverID=Mongo')
    LoginPrompt = False
    Left = 98
    Top = 109
  end
  object FDMongoQuery1: TFDMongoQuery
    FormatOptions.AssignedValues = [fvInlineDataSize, fvStrsTrim2Len]
    FormatOptions.InlineDataSize = 0
    UpdateOptions.KeyFields = '_id'
    Connection = MongoDBConnection
    DatabaseName = 'sample_analytics'
    CollectionName = 'customers'
    QProject = '{tier_and_details: 0}'
    Left = 96
    Top = 258
    object FDMongoQuery1_id: TWideStringField
      FieldName = '_id'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      FixedChar = True
      Size = 24
    end
    object FDMongoQuery1username: TWideStringField
      FieldName = 'username'
      Size = 16
    end
    object FDMongoQuery1name: TWideStringField
      FieldName = 'name'
      Size = 23
    end
    object FDMongoQuery1address: TWideStringField
      FieldName = 'address'
      Size = 57
    end
    object FDMongoQuery1birthdate: TDateTimeField
      FieldName = 'birthdate'
    end
    object FDMongoQuery1email: TWideStringField
      FieldName = 'email'
      Size = 29
    end
    object FDMongoQuery1active: TBooleanField
      FieldName = 'active'
    end
    object FDMongoQuery1accounts: TDataSetField
      FieldName = 'accounts'
    end
  end
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = FDMongoQuery1
    Left = 100
    Top = 331
  end
  object FDPhysMongoDriverLink1: TFDPhysMongoDriverLink
    Left = 95
    Top = 181
  end
end
