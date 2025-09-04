object FormDCSettings: TFormDCSettings
  Left = 0
  Top = 0
  Caption = 'DC Settings'
  ClientHeight = 259
  ClientWidth = 365
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
    Top = 78
    Width = 35
    Height = 13
    Caption = 'Key Id:'
  end
  object Label2: TLabel
    Left = 23
    Top = 124
    Width = 55
    Height = 13
    Caption = 'Key secret:'
  end
  object Label3: TLabel
    Left = 23
    Top = 170
    Width = 27
    Height = 13
    Caption = 'Data:'
  end
  object Label4: TLabel
    Left = 23
    Top = 15
    Width = 39
    Height = 13
    Caption = 'User Id:'
  end
  object eKeyId: TEdit
    Left = 23
    Top = 97
    Width = 226
    Height = 21
    TabOrder = 0
  end
  object eKeySecret: TEdit
    Left = 23
    Top = 143
    Width = 268
    Height = 21
    TabOrder = 1
  end
  object eData: TEdit
    Left = 23
    Top = 189
    Width = 334
    Height = 21
    TabOrder = 2
  end
  object bOk: TButton
    Left = 88
    Top = 226
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
  end
  object bCancel: TButton
    Left = 216
    Top = 226
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object eUserId: TEdit
    Left = 23
    Top = 34
    Width = 290
    Height = 21
    Enabled = False
    TabOrder = 5
  end
end
