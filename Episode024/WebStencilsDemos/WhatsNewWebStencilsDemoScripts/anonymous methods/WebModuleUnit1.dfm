object WebModule1: TWebModule1
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end>
  Height = 230
  Width = 415
  object WSProcessor: TWebStencilsProcessor
    InputFileName = '../../templates/BaseLayout.html'
    Left = 192
    Top = 80
  end
end
