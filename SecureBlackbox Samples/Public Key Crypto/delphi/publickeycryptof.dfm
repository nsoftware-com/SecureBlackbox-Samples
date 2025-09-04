object FormPublicKeyCrypto: TFormPublicKeyCrypto
  Left = 219
  Top = 154
  BorderStyle = bsDialog
  Caption = 'Public key crypto Demo'
  ClientHeight = 462
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label10: TLabel
    Left = 8
    Top = 8
    Width = 298
    Height = 13
    Caption = 'This sample shows how to detached sign and verify signature.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnGo: TButton
    Left = 403
    Top = 429
    Width = 70
    Height = 25
    Caption = 'Sign'
    Default = True
    TabOrder = 1
    OnClick = btnGoClick
  end
  object gbSettings: TGroupBox
    Left = 8
    Top = 40
    Width = 465
    Height = 369
    Caption = 'Settings'
    TabOrder = 0
    object lblInputFIle: TLabel
      Left = 16
      Top = 64
      Width = 73
      Height = 13
      Caption = 'Input filename:'
    end
    object lblPassword: TLabel
      Left = 16
      Top = 320
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object lblKeyFilename: TLabel
      Left = 16
      Top = 277
      Width = 123
      Height = 13
      Caption = 'Private key container file:'
    end
    object lblOutputFile: TLabel
      Left = 16
      Top = 120
      Width = 81
      Height = 13
      Caption = 'Output filename:'
    end
    object lblInputEncoding: TLabel
      Left = 16
      Top = 176
      Width = 47
      Height = 13
      Caption = 'Encoding:'
    end
    object lblKeyContainerType: TLabel
      Left = 16
      Top = 229
      Width = 95
      Height = 13
      Caption = 'Key container type:'
    end
    object lKeyAlg: TLabel
      Left = 152
      Top = 229
      Width = 69
      Height = 13
      Caption = 'Key algorithm:'
    end
    object lCurve: TLabel
      Left = 275
      Top = 229
      Width = 47
      Height = 13
      Caption = 'EC curve:'
    end
    object edInputFile: TEdit
      Left = 16
      Top = 84
      Width = 361
      Height = 21
      TabOrder = 0
    end
    object btnBrowseInputFile: TButton
      Left = 384
      Top = 80
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      TabOrder = 1
      OnClick = btnBrowseInputFileClick
    end
    object btnBrowseKey: TButton
      Left = 384
      Top = 291
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      TabOrder = 6
      OnClick = btnBrowseKeyClick
    end
    object edKeyFile: TEdit
      Left = 16
      Top = 293
      Width = 361
      Height = 21
      TabOrder = 5
    end
    object edPassphrase: TEdit
      Left = 16
      Top = 336
      Width = 177
      Height = 21
      PasswordChar = '*'
      TabOrder = 7
    end
    object btnBrowseOutputFile: TButton
      Left = 384
      Top = 133
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      TabOrder = 3
      OnClick = btnBrowseOutputFileClick
    end
    object edSignatureFile: TEdit
      Left = 16
      Top = 136
      Width = 361
      Height = 21
      TabOrder = 2
    end
    object cbEncoding: TComboBox
      Left = 16
      Top = 192
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'Binary'
      Items.Strings = (
        'Binary'
        'Base64'
        'Compact'
        'JSON')
    end
    object cbKeyContainerType: TComboBox
      Left = 16
      Top = 245
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 8
      Text = 'Generic private key'
      OnChange = cbKeyContainerTypeChange
      Items.Strings = (
        'Generic private key'
        'X.509 certificate')
    end
    object rbSign: TRadioButton
      Left = 16
      Top = 27
      Width = 73
      Height = 17
      Caption = 'Sign'
      Checked = True
      TabOrder = 9
      TabStop = True
      OnClick = rbSignClick
    end
    object rbVerify: TRadioButton
      Left = 95
      Top = 27
      Width = 74
      Height = 17
      Caption = 'Verify'
      TabOrder = 10
      OnClick = rbSignClick
    end
    object cbKeyAlg: TComboBox
      Left = 152
      Top = 245
      Width = 102
      Height = 21
      Style = csDropDownList
      TabOrder = 11
      OnChange = cbKeyAlgChange
      Items.Strings = (
        ''
        'RSA'
        'DSA'
        'EC'
        'ECDSA'
        'DH'
        'EDDSA')
    end
    object cbCurve: TComboBox
      Left = 275
      Top = 245
      Width = 102
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 12
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
  end
  object dlgOpenDialog: TOpenDialog
    Title = 'Please, select file'
    Left = 24
    Top = 408
  end
  object dlgSaveDialog: TSaveDialog
    Title = 'Please, select file'
    Left = 104
    Top = 408
  end
  object dlgOpenKey: TOpenDialog
    Filter = 'All key and certificate files (*.*)|*.*'
    FilterIndex = 0
    Title = 'Please, select key/certificate file'
    Left = 176
    Top = 408
  end
end


