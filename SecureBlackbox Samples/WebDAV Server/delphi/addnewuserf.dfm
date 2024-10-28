object FormAddnewuser: TFormAddnewuser
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add new user'
  ClientHeight = 170
  ClientWidth = 251
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
    Left = 16
    Top = 16
    Width = 29
    Height = 13
    Caption = 'Login:'
  end
  object Label2: TLabel
    Left = 16
    Top = 72
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object edLogin: TEdit
    Left = 16
    Top = 35
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object edPassword: TEdit
    Left = 16
    Top = 91
    Width = 217
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object bbOK: TButton
    Left = 48
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object bbCancel: TButton
    Left = 129
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
