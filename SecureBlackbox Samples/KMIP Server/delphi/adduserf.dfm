object Formadduser: TFormadduser
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add User'
  ClientHeight = 151
  ClientWidth = 284
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
  object edName: TEdit
    Left = 8
    Top = 27
    Width = 270
    Height = 21
    TabOrder = 0
  end
  object edPassword: TEdit
    Left = 8
    Top = 73
    Width = 270
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 156
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object bOk: TButton
    Left = 67
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
  end
end
