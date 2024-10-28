object FormChangeAuthResult: TFormChangeAuthResult
  Left = 0
  Top = 0
  Caption = 'Change auth method or/and result'
  ClientHeight = 241
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
  object Label2: TLabel
    Left = 15
    Top = 122
    Width = 122
    Height = 13
    Caption = 'Remaining auth methods:'
  end
  object eUserId: TEdit
    Left = 15
    Top = 34
    Width = 298
    Height = 21
    Enabled = False
    TabOrder = 0
  end
  object eAuthMethod: TEdit
    Left = 15
    Top = 80
    Width = 298
    Height = 21
    Enabled = False
    TabOrder = 1
  end
  object bOk: TButton
    Left = 68
    Top = 210
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
  end
  object bCancel: TButton
    Left = 196
    Top = 210
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object rbSucceeded: TRadioButton
    Left = 15
    Top = 175
    Width = 72
    Height = 17
    Caption = 'Succeeded'
    TabOrder = 4
  end
  object rbFailed: TRadioButton
    Left = 120
    Top = 175
    Width = 49
    Height = 17
    Caption = 'Failed'
    TabOrder = 5
  end
  object rbFurtherAuthNeeded: TRadioButton
    Left = 204
    Top = 175
    Width = 122
    Height = 17
    Caption = 'Further auth needed'
    TabOrder = 6
  end
  object eAuthMethods: TEdit
    Left = 15
    Top = 141
    Width = 298
    Height = 21
    TabOrder = 7
  end
end
