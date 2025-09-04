object FormAuthenticator: TFormAuthenticator
  Left = 0
  Top = 0
  Caption = 'Authenticator demo'
  ClientHeight = 223
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 216
    Height = 13
    Caption = 'This sample shows how to use authenticator.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 15
    Top = 56
    Width = 48
    Height = 13
    Caption = 'Users file:'
  end
  object Label2: TLabel
    Left = 15
    Top = 104
    Width = 97
    Height = 13
    Caption = 'Users file password:'
  end
  object bNew: TButton
    Left = 15
    Top = 175
    Width = 130
    Height = 35
    Caption = 'New authentication'
    TabOrder = 0
    OnClick = bNewClick
  end
  object bContinue: TButton
    Left = 190
    Top = 175
    Width = 130
    Height = 35
    Caption = 'Continue authentication'
    TabOrder = 1
    OnClick = bContinueClick
  end
  object eUsersFile: TEdit
    Left = 15
    Top = 75
    Width = 254
    Height = 21
    TabOrder = 2
  end
  object bUsersFile: TButton
    Left = 275
    Top = 73
    Width = 45
    Height = 25
    Caption = '...'
    TabOrder = 3
    OnClick = bUsersFileClick
  end
  object ePassword: TEdit
    Left = 15
    Top = 123
    Width = 178
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
  end
  object dlgOpen: TOpenDialog
    Left = 288
    Top = 8
  end
end


