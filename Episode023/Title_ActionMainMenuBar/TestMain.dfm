object frmTest: TfrmTest
  Left = 0
  Top = 0
  Caption = 'Test Main'
  ClientHeight = 507
  ClientWidth = 832
  Color = clBtnFace
  CustomTitleBar.Control = TitleBarPanel1
  CustomTitleBar.Enabled = True
  CustomTitleBar.Height = 35
  CustomTitleBar.SystemHeight = False
  CustomTitleBar.ShowCaption = False
  CustomTitleBar.StyleColors = True
  CustomTitleBar.SystemColors = False
  CustomTitleBar.SystemButtons = False
  CustomTitleBar.BackgroundColor = 16770250
  CustomTitleBar.ForegroundColor = 65793
  CustomTitleBar.InactiveBackgroundColor = clWhite
  CustomTitleBar.InactiveForegroundColor = 10066329
  CustomTitleBar.ButtonForegroundColor = 65793
  CustomTitleBar.ButtonBackgroundColor = 16770250
  CustomTitleBar.ButtonHoverForegroundColor = 65793
  CustomTitleBar.ButtonHoverBackgroundColor = 16053492
  CustomTitleBar.ButtonPressedForegroundColor = 65793
  CustomTitleBar.ButtonPressedBackgroundColor = 15395562
  CustomTitleBar.ButtonInactiveForegroundColor = 10066329
  CustomTitleBar.ButtonInactiveBackgroundColor = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  GlassFrame.Enabled = True
  GlassFrame.Top = 35
  Position = poScreenCenter
  StyleElements = [seFont, seClient]
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object TitleBarPanel1: TTitleBarPanel
    Left = 0
    Top = 0
    Width = 832
    Height = 34
    Margins.Bottom = 1
    CustomButtons = <>
    ExplicitWidth = 524
    object ActionMainMenuBar1: TActionMainMenuBar
      AlignWithMargins = True
      Left = 40
      Top = 2
      Width = 160
      Height = 29
      Margins.Left = 30
      Margins.Top = 2
      Margins.Bottom = 1
      UseSystemFont = False
      ActionManager = ActionManager1
      Align = alNone
      AnimationStyle = asSlide
      Caption = 'ActionMainMenuBar1'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 10461087
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = []
      Spacing = 0
      OnCanResize = ActionMainMenuBar1CanResize
    end
  end
  object Button1: TButton
    Left = 24
    Top = 56
    Width = 137
    Height = 35
    Caption = 'Change style...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ActionList1: TActionList
    Left = 40
    Top = 120
    object FileNew1: TAction
      Category = 'File'
      Caption = '&New'
      Hint = 'New|Create a new file'
      ImageName = 'Item7'
      ShortCut = 16462
      OnExecute = FileNew1Execute
    end
    object FileOpen1: TAction
      Category = 'File'
      Caption = '&Open'
      Hint = 'Open|Open a file'
      ImageName = 'Item8'
      ShortCut = 16463
      OnExecute = FileOpen1Execute
    end
    object FileClose1: TWindowClose
      Category = 'File'
      Caption = '&Close'
      Hint = 'Close|Close current file'
      OnExecute = FileClose1Execute
    end
    object FileSave1: TAction
      Category = 'File'
      Caption = '&Save'
      Hint = 'Save|Save current file'
      ImageName = 'Item9'
      ShortCut = 16467
      OnExecute = FileSave1Execute
    end
    object FileSaveAs1: TAction
      Category = 'File'
      Caption = 'Save &As...'
      Hint = 'Save As|Save current file with different name'
      OnExecute = FileSaveAs1Execute
    end
    object FileExit1: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Exit application'
      OnExecute = FileExit1Execute
    end
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageName = 'Item1'
      ShortCut = 16472
      OnExecute = EditCut1Execute
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageName = 'Item2'
      ShortCut = 16451
      OnExecute = EditCopy1Execute
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageName = 'Item3'
      ShortCut = 16470
      OnExecute = EditPaste1Execute
    end
    object WindowCascade1: TWindowCascade
      Category = 'Window'
      Caption = '&Cascade'
      Hint = 'Cascade'
      ImageName = 'Item18'
      OnExecute = WindowCascade1Execute
    end
    object WindowTileHorizontal1: TWindowTileHorizontal
      Category = 'Window'
      Caption = 'Tile &Horizontally'
      Hint = 'Tile Horizontally'
      ImageName = 'Item16'
      OnExecute = WindowTileHorizontal1Execute
    end
    object WindowTileVertical1: TWindowTileVertical
      Category = 'Window'
      Caption = 'Tile &Vertically'
      Hint = 'Tile Vertically'
      ImageName = 'Item17'
      OnExecute = WindowTileVertical1Execute
    end
    object WindowMinimizeAll1: TWindowMinimizeAll
      Category = 'Window'
      Caption = '&Minimize All'
      Hint = 'Minimize All'
      OnExecute = WindowMinimizeAll1Execute
    end
    object HelpAbout1: TAction
      Category = 'Help'
      Caption = '&About...'
      Hint = 
        'About|Displays program information, version number, and copyrigh' +
        't'
      OnExecute = HelpAbout1Execute
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = FileNew1
                ImageName = 'Item7'
                ShortCut = 16462
              end
              item
                Action = FileOpen1
                ImageName = 'Item8'
                ShortCut = 16463
              end
              item
                Action = FileClose1
              end
              item
                Action = FileSave1
                ImageName = 'Item9'
                ShortCut = 16467
              end
              item
                Action = FileSaveAs1
              end
              item
                Action = FileExit1
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = EditCut1
                ImageName = 'Item1'
                ShortCut = 16472
              end
              item
                Action = EditCopy1
                ImageName = 'Item2'
                ShortCut = 16451
              end
              item
                Action = EditPaste1
                ImageName = 'Item3'
                ShortCut = 16470
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = WindowCascade1
                ImageName = 'Item18'
              end
              item
                Action = WindowTileHorizontal1
                ImageName = 'Item16'
              end
              item
                Action = WindowTileVertical1
                ImageName = 'Item17'
              end
              item
                Action = WindowMinimizeAll1
              end>
            Caption = '&Window'
          end
          item
            Items = <
              item
                Action = HelpAbout1
              end>
            Caption = '&Help'
          end>
      end
      item
        Items = <
          item
            Items = <
              item
                Action = FileNew1
                ImageName = 'Item7'
                ShortCut = 16462
              end
              item
                Action = FileOpen1
                ImageName = 'Item8'
                ShortCut = 16463
              end
              item
                Action = FileClose1
              end
              item
                Action = FileSave1
                ImageName = 'Item9'
                ShortCut = 16467
              end
              item
                Action = FileSaveAs1
              end
              item
                Action = FileExit1
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = EditCut1
                ImageName = 'Item1'
                ShortCut = 16472
              end
              item
                Action = EditCopy1
                ImageName = 'Item2'
                ShortCut = 16451
              end
              item
                Action = EditPaste1
                ImageName = 'Item3'
                ShortCut = 16470
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = WindowCascade1
                ImageName = 'Item18'
              end
              item
                Action = WindowTileHorizontal1
                ImageName = 'Item16'
              end
              item
                Action = WindowTileVertical1
                ImageName = 'Item17'
              end
              item
                Action = WindowMinimizeAll1
              end>
            Caption = '&Window'
          end
          item
            Items = <
              item
                Action = HelpAbout1
              end>
            Caption = '&Help'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    LinkedActionLists = <
      item
        ActionList = ActionList1
        Caption = 'ActionList1'
      end>
    Left = 144
    Top = 120
    StyleName = 'Platform Default'
  end
end
