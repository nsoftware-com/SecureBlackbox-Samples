object FormContinueAuthentication: TFormContinueAuthentication
  Left = 0
  Top = 0
  Caption = 'Continue authentication'
  ClientHeight = 226
  ClientWidth = 594
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
  object Label1: TLabel
    Left = 23
    Top = 24
    Width = 55
    Height = 13
    Caption = 'Auth state:'
  end
  object Label2: TLabel
    Left = 23
    Top = 80
    Width = 66
    Height = 13
    Caption = 'Auth method:'
  end
  object Label3: TLabel
    Left = 23
    Top = 136
    Width = 57
    Height = 13
    Caption = 'Auth token:'
  end
  object eAuthState: TEdit
    Left = 23
    Top = 43
    Width = 393
    Height = 21
    TabOrder = 0
  end
  object eAuthMethod: TEdit
    Left = 23
    Top = 99
    Width = 154
    Height = 21
    Enabled = False
    TabOrder = 1
  end
  object bContinue: TButton
    Left = 511
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Continue'
    TabOrder = 2
    OnClick = bContinueClick
  end
  object cbAuthMethodsResult: TCheckBox
    Left = 23
    Top = 196
    Width = 226
    Height = 17
    Caption = 'Change remaining auth methods or result'
    TabOrder = 3
  end
  object bLoadState: TButton
    Left = 422
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 4
    OnClick = bLoadStateClick
  end
  object bSaveState: TButton
    Left = 511
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 5
    OnClick = bSaveStateClick
  end
  object eAuthToken: TEdit
    Left = 23
    Top = 155
    Width = 410
    Height = 21
    TabOrder = 6
  end
  object dlgOpen: TOpenDialog
    Left = 464
    Top = 96
  end
  object dlgSave: TSaveDialog
    Left = 536
    Top = 96
  end
end
