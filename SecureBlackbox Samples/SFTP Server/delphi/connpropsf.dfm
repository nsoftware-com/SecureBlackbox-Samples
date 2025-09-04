object FormConnprops: TFormConnprops
  Left = 205
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Server connection properties'
  ClientHeight = 129
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gbConnProps: TGroupBox
    Left = 8
    Top = 8
    Width = 249
    Height = 73
    Caption = 'Server connection properties'
    TabOrder = 0
    object lPort: TLabel
      Left = 16
      Top = 24
      Width = 66
      Height = 13
      Caption = 'Listen on port'
    end
    object editPort: TEdit
      Left = 16
      Top = 40
      Width = 73
      Height = 21
      TabOrder = 0
      Text = '21'
    end
  end
  object btnOK: TButton
    Left = 93
    Top = 94
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 182
    Top = 94
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
