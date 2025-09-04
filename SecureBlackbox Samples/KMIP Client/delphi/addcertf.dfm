object FormAddCert: TFormAddCert
  Left = 472
  Top = 447
  BorderStyle = bsDialog
  Caption = 'Adding certificate'
  ClientHeight = 156
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 81
    Top = 94
    Width = 32
    Height = 13
    Caption = 'Group:'
  end
  object Label2: TLabel
    Left = 47
    Top = 24
    Width = 66
    Height = 13
    Caption = 'Certificate file:'
  end
  object Label5: TLabel
    Left = 15
    Top = 59
    Width = 98
    Height = 13
    Caption = 'Certificate password:'
  end
  object btnOk: TButton
    Left = 281
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 362
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edCertFile: TEdit
    Left = 120
    Top = 20
    Width = 236
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object bOpenKeyFile: TButton
    Left = 362
    Top = 18
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = bOpenKeyFileClick
  end
  object edGroup: TEdit
    Left = 120
    Top = 90
    Width = 75
    Height = 21
    TabOrder = 4
  end
  object edCertPassword: TEdit
    Left = 120
    Top = 55
    Width = 199
    Height = 21
    PasswordChar = '*'
    TabOrder = 5
  end
  object dlgOpenFile: TOpenDialog
    Left = 410
    Top = 51
  end
end
