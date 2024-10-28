object FormChangeKey: TFormChangeKey
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Change entry password'
  ClientHeight = 137
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 26
    Top = 27
    Width = 69
    Height = 13
    Caption = 'Old password:'
  end
  object Label1: TLabel
    Left = 26
    Top = 67
    Width = 74
    Height = 13
    Caption = 'New password:'
  end
  object btnOK: TButton
    Left = 221
    Top = 105
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 318
    Top = 105
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edOldPassword: TEdit
    Left = 101
    Top = 24
    Width = 292
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object edNewPassword: TEdit
    Left = 101
    Top = 64
    Width = 292
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
end
