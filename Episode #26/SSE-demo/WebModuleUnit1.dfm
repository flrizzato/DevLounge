object WebModule1: TWebModule1
  Actions = <
    item
      Name = 'HealthAction'
      PathInfo = '/health'
      OnAction = WebModule1HealthAction
    end
    item
      Name = 'EventsAction'
      PathInfo = '/events'
      OnAction = WebModule1EventsAction
    end
    item
      Name = 'StartJobAction'
      PathInfo = '/startjob'
      OnAction = WebModule1StartJobAction
    end
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end>
  Height = 288
  Width = 519
  PixelsPerInch = 120
  object WebStencilsProcessor1: TWebStencilsProcessor
    Left = 96
    Top = 64
  end
end
