object FormProgress: TFormProgress
  Left = 395
  Top = 228
  BorderStyle = bsDialog
  Caption = 'Extracting...'
  ClientHeight = 196
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
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
    Width = 340
    Height = 16
    TabOrder = 0
  end
  object pbTotalProgress: TProgressBar
    Left = 8
    Top = 64
    Width = 340
    Height = 16
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 135
    Top = 85
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object lvLog: TListView
    Left = 0
    Top = 119
    Width = 354
    Height = 77
    Align = alBottom
    Columns = <
      item
        Width = 60
      end
      item
        AutoSize = True
      end>
    TabOrder = 3
    ViewStyle = vsReport
  end
end
