object FormSetpublickey: TFormSetpublickey
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Set public key'
  ClientHeight = 228
  ClientWidth = 338
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
  object gbSetPublicKey: TGroupBox
    Left = 8
    Top = 8
    Width = 321
    Height = 177
    Caption = 'Set public key'
    TabOrder = 0
    object lPublicKey: TLabel
      Left = 16
      Top = 24
      Width = 140
      Height = 13
      Caption = 'Please insert public key here:'
    end
    object memoPublicKey: TMemo
      Left = 16
      Top = 40
      Width = 289
      Height = 113
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 165
    Top = 195
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 254
    Top = 195
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
