object FormOfficeencryptor: TFormOfficeencryptor
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Office Encryptor demo'
  ClientHeight = 440
  ClientWidth = 706
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
  object lDemoInfo: TLabel
    Left = 8
    Top = 8
    Width = 436
    Height = 13
    Caption = 
      'This sample illustrates the use of OfficeEncryptor component to ' +
      'encrypt office documents. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lbInputFile: TLabel
    Left = 10
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object lOutputFile: TLabel
    Left = 9
    Top = 83
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object btnBrowseInput: TButton
    Left = 321
    Top = 44
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 1
    OnClick = btnBrowseInputClick
  end
  object btnBrowseOutput: TButton
    Left = 321
    Top = 79
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 3
    OnClick = btnBrowseOutputClick
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 245
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 80
    Width = 245
    Height = 21
    TabOrder = 2
  end
  object gbEncryptionOptions: TGroupBox
    Left = 6
    Top = 120
    Width = 689
    Height = 101
    Caption = 'Encryption options'
    TabOrder = 4
    object Label4: TLabel
      Left = 10
      Top = 29
      Width = 82
      Height = 13
      Caption = 'Encryption Type:'
    end
    object lEncryptionAlgorithm: TLabel
      Left = 275
      Top = 29
      Width = 103
      Height = 13
      Caption = 'Encryption Algorithm:'
    end
    object lPassword: TLabel
      Left = 10
      Top = 67
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object cbEncryptionType: TComboBox
      Left = 98
      Top = 25
      Width = 135
      Height = 22
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Default'
      Items.Strings = (
        'Default'
        'BinaryRC4'
        'BinaryRC4CryptoAPI'
        'OpenXMLStandard'
        'OpenXMLAgile'
        'OpenOffice')
    end
    object cbEncryptionAlgorithm: TComboBox
      Left = 384
      Top = 25
      Width = 135
      Height = 22
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Default'
      Items.Strings = (
        'Default'
        'RC2'
        'RC4'
        'DES'
        '3DES'
        'AES128'
        'AES192'
        'AES256'
        'Blowfish')
    end
    object edPassword: TEdit
      Left = 98
      Top = 64
      Width = 135
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
  end
  object btnEncrypt: TButton
    Left = 620
    Top = 399
    Width = 70
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Encrypt'
    TabOrder = 8
    OnClick = btnEncryptClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 227
    Width = 225
    Height = 162
    Caption = 'Additional options: RC4 Crypto API'
    TabOrder = 5
    object lRC4KeyBits: TLabel
      Left = 10
      Top = 88
      Width = 94
      Height = 13
      Caption = 'RC4 Key Size in Bits'
    end
    object cbEncryptDocumentProperties: TCheckBox
      Left = 10
      Top = 30
      Width = 200
      Height = 17
      Caption = 'Encrypt Document Properties'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbHardenedKeyGeneration: TCheckBox
      Left = 10
      Top = 57
      Width = 200
      Height = 17
      Caption = 'Hardened Key Generation'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object edRC4KeyBits: TEdit
      Left = 10
      Top = 107
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '128'
    end
  end
  object GroupBox2: TGroupBox
    Left = 239
    Top = 227
    Width = 225
    Height = 162
    Caption = 'Additional options: OpenXML Agile'
    TabOrder = 6
    object lSaltSize: TLabel
      Left = 11
      Top = 64
      Width = 44
      Height = 13
      Caption = 'Salt Size:'
    end
    object lSpinCount: TLabel
      Left = 11
      Top = 112
      Width = 56
      Height = 13
      Caption = 'Spin Count:'
    end
    object lHashAlgorithm: TLabel
      Left = 10
      Top = 23
      Width = 76
      Height = 13
      Caption = 'Hash Algorithm:'
    end
    object edSaltSize: TEdit
      Left = 12
      Top = 83
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '16'
    end
    object edSpinCount: TEdit
      Left = 12
      Top = 131
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '100000'
    end
    object cbHashALgorithm: TComboBox
      Left = 10
      Top = 36
      Width = 135
      Height = 22
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 0
      Text = 'SHA256'
      Items.Strings = (
        'SHA1'
        'SHA256'
        'SHA384'
        'SHA512'
        'MD5'
        'RIPEMD160')
    end
  end
  object GroupBox3: TGroupBox
    Left = 470
    Top = 227
    Width = 225
    Height = 162
    Caption = 'Additional options: OpenDocument'
    TabOrder = 7
    object lChecksumAlgorithm: TLabel
      Left = 10
      Top = 23
      Width = 100
      Height = 13
      Caption = 'Checksum Algorithm:'
    end
    object cbChecksumAlgorithm: TComboBox
      Left = 10
      Top = 36
      Width = 135
      Height = 22
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 0
      Text = 'SHA256'
      Items.Strings = (
        'SHA1'
        'SHA256')
    end
  end
  object dlgOpen: TOpenDialog
    Left = 496
    Top = 40
  end
  object dlgSave: TSaveDialog
    Left = 568
    Top = 40
  end
end


