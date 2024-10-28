object Formadduser: TFormadduser
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add User'
  ClientHeight = 202
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = 'UserName'
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object lbPrivateKey: TLabel
    Left = 8
    Top = 108
    Width = 182
    Height = 13
    Caption = 'Public key file KEY authentication type'
    FocusControl = edPublicKey
  end
  object sbPublicKey: TSpeedButton
    Left = 245
    Top = 122
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbPublicKeyClick
  end
  object edName: TEdit
    Left = 8
    Top = 27
    Width = 273
    Height = 21
    TabOrder = 0
  end
  object edPassword: TEdit
    Left = 8
    Top = 73
    Width = 273
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 172
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 83
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 4
    OnClick = Button2Click
  end
  object edPublicKey: TEdit
    Left = 8
    Top = 124
    Width = 231
    Height = 21
    TabOrder = 2
  end
  object OpenDialog: TOpenDialog
    Left = 272
    Top = 16
  end
end
