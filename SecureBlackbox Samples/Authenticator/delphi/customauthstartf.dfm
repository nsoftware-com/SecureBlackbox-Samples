object FormCustomAuthStart: TFormCustomAuthStart
  Left = 0
  Top = 0
  Caption = 'Custom authentication start'
  ClientHeight = 216
  ClientWidth = 323
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
    Top = 119
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
  object eAuthMethodData: TEdit
    Left = 15
    Top = 138
    Width = 290
    Height = 21
    TabOrder = 0
  end
  object bOk: TButton
    Left = 64
    Top = 183
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 192
    Top = 183
    Width = 75
    Height = 25
    Caption = 'Cancel'
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
end
