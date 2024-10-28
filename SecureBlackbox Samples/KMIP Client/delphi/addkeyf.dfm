object FormAddKey: TFormAddKey
  Left = 472
  Top = 447
  BorderStyle = bsDialog
  Caption = 'Adding key'
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
    Left = 20
    Top = 59
    Width = 32
    Height = 13
    Caption = 'Group:'
  end
  object Label7: TLabel
    Left = 21
    Top = 94
    Width = 31
    Height = 13
    Caption = 'Curve:'
  end
  object Label2: TLabel
    Left = 15
    Top = 24
    Width = 37
    Height = 13
    Caption = 'Key file:'
  end
  object btnOk: TButton
    Left = 280
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 361
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edKeyFile: TEdit
    Left = 60
    Top = 20
    Width = 295
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object bOpenKeyFile: TButton
    Left = 361
    Top = 18
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = bOpenKeyFileClick
  end
  object edGroup: TEdit
    Left = 60
    Top = 55
    Width = 165
    Height = 21
    TabOrder = 4
  end
  object cbCurve: TComboBox
    Left = 60
    Top = 90
    Width = 151
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 5
    Items.Strings = (
      ''
      'SECP112R1'
      'SECT113R1'
      'SECP128R1'
      'SECT131R1'
      'SECP160K1'
      'SECT163K1'
      'C2PNB176W1'
      'C2TNB191V1'
      'SECP192K1'
      'SECT193R1'
      'C2PNB208W1'
      'SECP224K1'
      'SECT233K1'
      'SECT239K1'
      'SECP256K1'
      'C2PNB272W1'
      'SECT283K1'
      'C2PNB304W1'
      'C2TNB359V1'
      'C2PNB368W1'
      'SECP384R1'
      'SECT409K1'
      'C2TNB431R1'
      'BRAINPOOLP512R1'
      'SECP521R1'
      'SECT571K1')
  end
  object dlgOpenFile: TOpenDialog
    Left = 402
    Top = 67
  end
end
