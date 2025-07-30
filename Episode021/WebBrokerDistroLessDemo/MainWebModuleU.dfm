object MainWebModule: TMainWebModule
  Actions = <>
  Height = 361
  Width = 429
  object WebStencilsEngine: TWebStencilsEngine
    Dispatcher = WebFileDispatcher
    PathTemplates = <
      item
        Template = '/'
        Redirect = '/home.html'
      end
      item
        Template = '/{filename}'
      end>
    RootDirectory = '../../html/'
    OnValue = WebStencilsEngineValue
    Left = 64
    Top = 24
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
    Left = 64
    Top = 88
  end
  object Customers: TFDQuery
    Connection = Connection
    SQL.Strings = (
      'SELECT *'
      'FROM customers')
    Left = 341
    Top = 24
  end
  object Connection: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\azapater\Documents\GitHub\WebStencilsDemos\Web' +
        'StencilsDemoProject\resources\data\database.sqlite3'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 253
    Top = 24
  end
end
