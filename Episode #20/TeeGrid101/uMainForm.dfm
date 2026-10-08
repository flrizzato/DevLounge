object MainForm: TMainForm
  Left = 0
  Top = 0
  Margins.Left = 4
  Margins.Top = 4
  Margins.Right = 4
  Margins.Bottom = 4
  Caption = 'TeeGrid101'
  ClientHeight = 623
  ClientWidth = 1107
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  DesignSize = (
    1107
    623)
  TextHeight = 20
  object TeeGrid1: TTeeGrid
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 1097
    Height = 398
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 220
    Cells.Format.Font.Name = 'Segoe UI'
    Cells.Format.Font.Size = 11.000000000000000000
    CellFormat = <
      item
        Format.Font.Size = 10.000000000000000000
      end>
    Columns = <
      item
        Header.Text = 'EMP_NO'
        Link = 'EMP_NO'
      end
      item
        Header.Text = 'FIRST_NAME'
        Link = 'FIRST_NAME'
      end
      item
        Header.Text = 'LAST_NAME'
        Link = 'LAST_NAME'
      end
      item
        Header.Text = 'PHONE_EXT'
        Link = 'PHONE_EXT'
      end
      item
        Header.Text = 'HIRE_DATE'
        Link = 'HIRE_DATE'
      end
      item
        Header.Text = 'DEPT_NO'
        Link = 'DEPT_NO'
      end
      item
        Header.Text = 'JOB_CODE'
        Link = 'JOB_CODE'
      end
      item
        Header.Text = 'JOB_GRADE'
        Link = 'JOB_GRADE'
      end
      item
        Header.Text = 'JOB_COUNTRY'
        Link = 'JOB_COUNTRY'
      end
      item
        Header.Text = 'SALARY'
        Link = 'SALARY'
      end
      item
        Header.Text = 'FULL_NAME'
        Link = 'FULL_NAME'
      end>
    DataSource = MainDM.EmployeeTable
    Header.Format.Stroke.Brush.Color = x00999999
    Header.Format.Font.Name = 'Segoe UI'
    Header.Format.Font.Size = 11.000000000000000000
    Header.Lines.Brush.Color = x00999999
    Header.Hover.ParentFont = False
    Header.RowLines.Brush.Color = x00999999
    Indicator.Format.Brush.Color = x00F3F3F3
    Indicator.Format.Stroke.Brush.Color = x00999999
    Indicator.Visible = False
    Rows.Format.Font.Name = 'Segoe UI'
    Rows.Format.Font.Size = 11.000000000000000000
    Rows.Lines.Brush.Color = x00999999
    Rows.Alternate.Brush.Color = x00F3F3F3
    Rows.Alternate.Stroke.Brush.Color = x00999999
    Rows.Hover.Format.Font.Name = 'Segoe UI'
    Rows.Hover.Format.Font.Size = 11.000000000000000000
    Rows.RowLines.Brush.Color = x00999999
    Selected.Format.Font.Name = 'Segoe UI'
    Selected.Format.Font.Size = 11.000000000000000000
    Selected.ParentFont = False
    Selected.UnFocused.Format.Font.Name = 'Segoe UI'
    Selected.UnFocused.Format.Font.Size = 11.000000000000000000
    Align = alClient
    ParentFont = True
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 0
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
          Format.Stroke.Brush.Color = x00999999
          Format.Font.Name = 'Segoe UI'
          Format.Font.Size = 11.000000000000000000
          Lines.Brush.Color = x00999999
          Hover.ParentFont = False
          RowLines.Brush.Color = x00999999
        end>)
  end
  object Button1: TButton
    Left = 9
    Top = 427
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Apply Theme'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 9
    Top = 466
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Per Column Format'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 9
    Top = 505
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Per Cell Format'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 9
    Top = 544
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Lock Column'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 9
    Top = 583
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Row Height'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 172
    Top = 427
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Row Groups'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 172
    Top = 466
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Range Selection'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 172
    Top = 505
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Range Copy->CSV'
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 172
    Top = 544
    Width = 155
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = 'Add Totals'
    TabOrder = 9
    OnClick = Button9Click
  end
end
