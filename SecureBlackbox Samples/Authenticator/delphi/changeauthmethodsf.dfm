object FormChangeAuthMethods: TFormChangeAuthMethods
  Left = 0
  Top = 0
  Caption = 'Change authentication methods'
  ClientHeight = 158
  ClientWidth = 334
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
  object Label2: TLabel
    Left = 15
    Top = 64
    Width = 71
    Height = 13
    Caption = 'Auth methods:'
  end
  object Label1: TLabel
    Left = 15
    Top = 15
    Width = 39
    Height = 13
    Caption = 'User Id:'
  end
  object eAuthMethods: TEdit
    Left = 15
    Top = 83
    Width = 186
    Height = 21
    TabOrder = 0
  end
  object bBaseMethods: TButton
    Left = 212
    Top = 81
    Width = 100
    Height = 25
    Caption = 'Base auth methods'
    TabOrder = 1
  end
  object bOk: TButton
    Left = 64
    Top = 125
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
  end
  object bCancel: TButton
    Left = 192
    Top = 125
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object eUserId: TEdit
    Left = 15
    Top = 34
    Width = 290
    Height = 21
    Enabled = False
    TabOrder = 4
  end
end
