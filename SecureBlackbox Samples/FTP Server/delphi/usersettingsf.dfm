object FormUsersettings: TFormUsersettings
  Left = 429
  Top = 236
  BorderStyle = bsDialog
  Caption = 'User settings'
  ClientHeight = 182
  ClientWidth = 327
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lUser: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Username:'
  end
  object lPassword: TLabel
    Left = 192
    Top = 8
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object lHomeDirectory: TLabel
    Left = 8
    Top = 48
    Width = 77
    Height = 13
    Caption = 'Home directory:'
  end
  object lSpeedLimit: TLabel
    Left = 8
    Top = 96
    Width = 55
    Height = 13
    Caption = 'Speed limit:'
  end
  object lKBSec: TLabel
    Left = 112
    Top = 118
    Width = 32
    Height = 13
    Caption = 'Kb/sec'
  end
  object btnOK: TButton
    Left = 160
    Top = 152
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 246
    Top = 152
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object editUsername: TEdit
    Left = 8
    Top = 24
    Width = 177
    Height = 21
    TabOrder = 2
  end
  object editPassword: TEdit
    Left = 192
    Top = 24
    Width = 129
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object editHomeDirectory: TEdit
    Left = 8
    Top = 64
    Width = 313
    Height = 21
    TabOrder = 4
  end
  object editSpeedLimit: TEdit
    Left = 8
    Top = 112
    Width = 97
    Height = 21
    TabOrder = 5
  end
end
