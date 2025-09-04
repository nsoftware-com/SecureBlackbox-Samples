object FormAuthVerify: TFormAuthVerify
  Left = 0
  Top = 0
  Caption = 'Authentication verify'
  ClientHeight = 259
  ClientWidth = 322
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
    Top = 107
    Width = 91
    Height = 13
    Caption = 'Auth method data:'
  end
  object Label1: TLabel
    Left = 15
    Top = 15
    Width = 39
    Height = 13
    Caption = 'User Id:'
  end
  object Label3: TLabel
    Left = 15
    Top = 61
    Width = 66
    Height = 13
    Caption = 'Auth method:'
  end
  object Label4: TLabel
    Left = 15
    Top = 153
    Width = 57
    Height = 13
    Caption = 'Auth token:'
  end
  object eAuthMethodData: TEdit
    Left = 15
    Top = 126
    Width = 290
    Height = 21
    Enabled = False
    TabOrder = 0
  end
  object bValid: TButton
    Left = 64
    Top = 219
    Width = 75
    Height = 25
    Caption = 'Valid'
    ModalResult = 1
    TabOrder = 1
  end
  object bNotValid: TButton
    Left = 192
    Top = 219
    Width = 75
    Height = 25
    Caption = 'Not valid'
    ModalResult = 2
    TabOrder = 2
  end
  object eUserId: TEdit
    Left = 15
    Top = 34
    Width = 290
    Height = 21
    Enabled = False
    TabOrder = 3
  end
  object eAuthMethod: TEdit
    Left = 15
    Top = 80
    Width = 290
    Height = 21
    Enabled = False
    TabOrder = 4
  end
  object eAuthToken: TEdit
    Left = 15
    Top = 172
    Width = 290
    Height = 21
    Enabled = False
    TabOrder = 5
  end
end
