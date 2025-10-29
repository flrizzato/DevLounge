object WebstencilsResource1: TWebstencilsResource1
  Height = 299
  Width = 410
  object html: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 40
    Top = 24
  end
  object WebStencilsEngine1: TWebStencilsEngine
    Dispatcher = html
    PathTemplates = <
      item
        Template = '/{filename}'
      end
      item
        Template = '/'
        Redirect = '/home.html'
      end>
    RootDirectory = 
      'C:\Users\azapater\Documents\GitHub\InternalWebStencilsDemos\WebS' +
      'tencilsRADServerProject\html\'
    Left = 216
    Top = 24
  end
  object WebStencilsProcessor: TWebStencilsProcessor
    Engine = WebStencilsEngine1
    Left = 216
    Top = 88
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'CharacterSet=UTF8'
      'DriverID=IB')
    LoginPrompt = False
    Left = 328
    Top = 24
  end
  object customers: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'SELECT *'
      'FROM customers')
    Left = 328
    Top = 88
  end
  object css: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 40
    Top = 88
  end
  object js: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 40
    Top = 152
  end
  object img: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 40
    Top = 216
  end
end
