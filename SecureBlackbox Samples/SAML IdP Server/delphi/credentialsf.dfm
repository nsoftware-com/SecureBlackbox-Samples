object FormCredentials: TFormCredentials
  Left = 0
  Top = 0
  Caption = 'New User'
  ClientHeight = 202
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 29
    Height = 13
    Caption = 'Login:'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 32
    Height = 13
    Caption = 'E-mail:'
  end
  object edLogin: TEdit
    Left = 8
    Top = 27
    Width = 260
    Height = 21
    TabOrder = 0
  end
  object edPassword: TEdit
    Left = 8
    Top = 75
    Width = 260
    Height = 21
    TabOrder = 1
  end
  object bbOK: TButton
    Left = 64
    Top = 170
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object bbCancel: TButton
    Left = 145
    Top = 170
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object edEmail: TEdit
    Left = 8
    Top = 123
    Width = 260
    Height = 21
    TabOrder = 4
  end
end
