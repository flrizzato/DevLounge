object WebModule1: TWebModule1
  Actions = <>
  Height = 338
  Width = 519
  PixelsPerInch = 120
  object WebSessionManager: TWebSessionManager
    Left = 80
    Top = 40
  end
  object WebFormsAuthenticator: TWebFormsAuthenticator
    LoginURL = '/login'
    HomeURL = '/'
    LogoutURL = '/logout'
    OnAuthenticate = WebFormsAuthenticatorAuthenticate
    Left = 80
    Top = 198
  end
  object WebAuthorizer: TWebAuthorizer
    UnauthorizedURL = '/forbidden'
    Zones = <
      item
        PathInfo = '/'
        Kind = zkFree
      end
      item
        PathInfo = '/authenticated*'
      end
      item
        PathInfo = '/admin*'
        Roles = 'admin'
      end>
    Left = 80
    Top = 120
  end
  object WSEngine: TWebStencilsEngine
    Dispatcher = WebFileDispatcher1
    PathTemplates = <
      item
        Template = '/'
        Redirect = '/home.html'
      end
      item
        Template = '/{filename}'
      end>
    RootDirectory = '..\..\templates\'
    Left = 240
    Top = 40
  end
  object WebFileDispatcher1: TWebFileDispatcher
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
    RootDirectory = '/../../templates/'
    VirtualPath = '/'
    Left = 390
    Top = 40
  end
end
