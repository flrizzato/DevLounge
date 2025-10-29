object TasksResource1: TTasksResource1
  OnCreate = DataModuleCreate
  Height = 377
  Width = 438
  object html: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 56
    Top = 40
  end
  object WebStencilsEngine1: TWebStencilsEngine
    Dispatcher = html
    PathTemplates = <
      item
        Template = '/{filename}'
      end
      item
        Template = '/examples/{filename}'
      end>
    RootDirectory = 
      'C:\Users\azapater\Documents\GitHub\InternalWebStencilsDemos\WebS' +
      'tencilsRADServerProject\html\'
    OnValue = WebStencilsEngine1Value
    Left = 232
    Top = 32
  end
  object WebStencilsProcessor: TWebStencilsProcessor
    Engine = WebStencilsEngine1
    OnValue = WebStencilsEngine1Value
    Left = 232
    Top = 104
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'CharacterSet=UTF8'
      'DriverID=IB')
    LoginPrompt = False
    Left = 344
    Top = 40
  end
  object customers: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'SELECT *'
      'FROM customers')
    Left = 344
    Top = 104
  end
  object css: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 56
    Top = 104
  end
  object js: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 56
    Top = 168
  end
  object img: TEMSFileResource
    PathTemplate = 'C:\Users\azapater\Desktop\test\html\{filename}'
    Left = 56
    Top = 232
  end
end
