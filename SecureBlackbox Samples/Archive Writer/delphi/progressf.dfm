object FormProgress: TFormProgress
  Left = 395
  Top = 228
  BorderStyle = bsSingle
  Caption = 'Extracting...'
  ClientHeight = 205
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCanResize = FormCanResize
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblCurrentFile: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Current file:'
  end
  object lblTotal: TLabel
    Left = 8
    Top = 48
    Width = 27
    Height = 13
    Caption = 'Total:'
  end
  object lblCurrentFileName: TLabel
    Left = 67
    Top = 8
    Width = 300
    Height = 13
    AutoSize = False
  end
  object pbCurrentFileProgress: TProgressBar
    Left = 8
    Top = 24
    Width = 393
    Height = 16
    TabOrder = 0
  end
  object pbTotalProgress: TProgressBar
    Left = 8
    Top = 64
    Width = 393
    Height = 16
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 120
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 208
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 3
    OnClick = btnOkClick
  end
  object lvLog: TListView
    Left = 0
    Top = 128
    Width = 405
    Height = 77
    Align = alBottom
    Columns = <
      item
        Width = 60
      end
      item
        AutoSize = True
      end>
    TabOrder = 4
    ViewStyle = vsReport
  end
end
