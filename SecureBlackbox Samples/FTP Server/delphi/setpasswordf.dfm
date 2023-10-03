object FormSetpassword: TFormSetpassword
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Set password'
  ClientHeight = 181
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbSetPassword: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 129
    Caption = 'Setting password for user'
    TabOrder = 0
    object lPass: TLabel
      Left = 16
      Top = 24
      Width = 74
      Height = 13
      Caption = 'New password:'
    end
    object lPassConf: TLabel
      Left = 16
      Top = 72
      Width = 112
      Height = 13
      Caption = 'Password confirmation:'
    end
    object editPassword: TEdit
      Left = 16
      Top = 40
      Width = 233
      Height = 21
      PasswordChar = '*'
      TabOrder = 0
    end
    object editPasswordConf: TEdit
      Left = 16
      Top = 88
      Width = 233
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 109
    Top = 148
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 198
    Top = 148
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
