object FormNewAuthentication: TFormNewAuthentication
  Left = 0
  Top = 0
  Caption = 'New authentication'
  ClientHeight = 161
  ClientWidth = 424
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
    Left = 23
    Top = 16
    Width = 39
    Height = 13
    Caption = 'User Id:'
  end
  object Label2: TLabel
    Left = 23
    Top = 72
    Width = 108
    Height = 13
    Caption = 'Default auth methods:'
  end
  object eUserId: TEdit
    Left = 23
    Top = 35
    Width = 146
    Height = 21
    TabOrder = 0
  end
  object eDefAuthMethods: TEdit
    Left = 23
    Top = 91
    Width = 186
    Height = 21
    TabOrder = 1
  end
  object bBaseMethods: TButton
    Left = 220
    Top = 89
    Width = 100
    Height = 25
    Caption = 'Base auth methods'
    TabOrder = 2
    OnClick = bBaseMethodsClick
  end
  object bStart: TButton
    Left = 341
    Top = 130
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 3
    OnClick = bStartClick
  end
end
