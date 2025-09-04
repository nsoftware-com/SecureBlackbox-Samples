object FormCreateKey: TFormCreateKey
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FormCreateKey'
  ClientHeight = 176
  ClientWidth = 499
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
  object GroupBox1: TGroupBox
    Left = 5
    Top = 5
    Width = 485
    Height = 125
    Caption = 'Parameters  '
    TabOrder = 0
    object Label1: TLabel
      Left = 60
      Top = 98
      Width = 33
      Height = 13
      Caption = 'Group:'
    end
    object Label2: TLabel
      Left = 15
      Top = 28
      Width = 78
      Height = 13
      Caption = 'Public algorithm:'
    end
    object lblSelectPublicKeyLen: TLabel
      Left = 35
      Top = 64
      Width = 58
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = 'Key length: '
      ParentBiDiMode = False
    end
    object lblCurve: TLabel
      Left = 250
      Top = 64
      Width = 77
      Height = 13
      Caption = 'Curve (for EC) :'
      Enabled = False
    end
    object edGroup: TEdit
      Left = 100
      Top = 95
      Width = 145
      Height = 21
      TabOrder = 0
    end
    object cbPublicAlgorithm: TComboBox
      Left = 100
      Top = 25
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'RSA'
      OnChange = cbPublicAlgorithmChange
      Items.Strings = (
        'RSA'
        'DSA'
        'EC'
        'DES'
        '3DES'
        'AES')
    end
    object cbCurve: TComboBox
      Left = 335
      Top = 60
      Width = 122
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemIndex = 0
      TabOrder = 2
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
    object edKeyLength: TEdit
      Left = 100
      Top = 60
      Width = 69
      Height = 21
      TabOrder = 3
      Text = '1024'
    end
  end
  object btnOk: TButton
    Left = 326
    Top = 142
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 415
    Top = 142
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
