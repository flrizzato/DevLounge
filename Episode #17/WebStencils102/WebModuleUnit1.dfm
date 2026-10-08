object WebModule1: TWebModule1
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end>
  Height = 288
  Width = 519
  PixelsPerInch = 120
  object WebStencilsEngine1: TWebStencilsEngine
    PathTemplates = <
      item
        Template = '/{filename}'
      end>
    RootDirectory = '..\..\html\'
    Left = 96
    Top = 32
  end
  object WebStencilsProcessor1: TWebStencilsProcessor
    Engine = WebStencilsEngine1
    InputFileName = '..\..\html\index.html'
    Left = 96
    Top = 120
  end
  object MastsqlConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=MASTSQL')
    LoginPrompt = False
    Left = 304
    Top = 28
  end
  object PartsTable: TFDQuery
    Connection = MastsqlConnection
    SQL.Strings = (
      'SELECT * FROM PARTS ORDER BY PARTNO')
    Left = 304
    Top = 116
    object PartsTablePARTNO: TFloatField
      FieldName = 'PARTNO'
      Origin = 'PARTNO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object PartsTableVENDORNO: TFloatField
      FieldName = 'VENDORNO'
      Origin = 'VENDORNO'
      Required = True
    end
    object PartsTableDESCRIPTION: TStringField
      FieldName = 'DESCRIPTION'
      Origin = 'DESCRIPTION'
      Required = True
      Size = 30
    end
    object PartsTableONHAND: TFloatField
      FieldName = 'ONHAND'
      Origin = 'ONHAND'
    end
    object PartsTableONORDER: TFloatField
      FieldName = 'ONORDER'
      Origin = 'ONORDER'
    end
    object PartsTableCOST: TFloatField
      FieldName = 'COST'
      Origin = 'COST'
    end
    object PartsTableLISTPRICE: TFloatField
      FieldName = 'LISTPRICE'
      Origin = 'LISTPRICE'
    end
  end
  object WebFileDispatcher: TWebFileDispatcher
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/html'
        Extensions = 'html;htm'
      end
      item
        MimeType = 'application/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpeg;jpg'
      end
      item
        MimeType = 'image/png'
        Extensions = 'png'
      end
      item
        MimeType = 'image/svg+xml'
        Extensions = 'svg;svgz'
      end
      item
        MimeType = 'image/x-icon'
        Extensions = 'ico'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '*'
      end
      item
        DirectoryAction = dirExclude
        DirectoryMask = '\templates\*'
      end>
    RootDirectory = '../../html/'
    VirtualPath = '/'
    Left = 96
    Top = 198
  end
end
