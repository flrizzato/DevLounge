object WebModule1: TWebModule1
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end
    item
      Name = 'WebActionItem1'
      PathInfo = '/link1'
      OnAction = WebModule1WebActionItem1Action
    end
    item
      Name = 'WebActionItem2'
      PathInfo = '/link2'
      OnAction = WebModule1WebActionItem2Action
    end>
  Height = 230
  Width = 415
  object WSProcessor: TWebStencilsProcessor
    InputFileName = '../../templates/BaseLayout.html'
    Left = 192
    Top = 80
  end
end
