object FormXmlencryptor: TFormXmlencryptor
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'XML Encryptor demo'
  ClientHeight = 727
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    381
    727)
  PixelsPerInch = 96
  TextHeight = 14
  object lSourceFile: TLabel
    Left = 8
    Top = 60
    Width = 76
    Height = 13
    Caption = 'Source XML file:'
  end
  object lDestFile: TLabel
    Left = 8
    Top = 108
    Width = 97
    Height = 13
    Caption = 'Destination XML file:'
  end
  object lDemoInfo: TLabel
    Left = 8
    Top = 8
    Width = 355
    Height = 46
    AutoSize = False
    Caption = 
      'This sample illustrates the use of XMLEncryptor component for en' +
      'crypting XML documents. Please pick the encryption settings and ' +
      'specify either encryption password or certificate and then click' +
      ' '#39'Encrypt'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object edSource: TEdit
    Left = 8
    Top = 79
    Width = 281
    Height = 21
    TabOrder = 0
  end
  object edDest: TEdit
    Left = 8
    Top = 127
    Width = 281
    Height = 21
    TabOrder = 2
  end
  object btnBrowseSource: TButton
    Left = 295
    Top = 75
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseSourceClick
  end
  object btnBrowseDest: TButton
    Left = 295
    Top = 123
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btnBrowseDestClick
  end
  object gbEncryptionProps: TGroupBox
    Left = 8
    Top = 542
    Width = 365
    Height = 141
    Caption = 'Key/certificate properties'
    TabOrder = 6
    object lbCertificate: TLabel
      Left = 16
      Top = 64
      Width = 106
      Height = 13
      Caption = 'Encryption certificate:'
      Enabled = False
    end
    object lbCertPassword: TLabel
      Left = 15
      Top = 110
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
      Enabled = False
    end
    object lbPassphrase: TLabel
      Left = 16
      Top = 30
      Width = 72
      Height = 13
      Caption = 'Key/Password:'
    end
    object edPassphrase: TEdit
      Left = 223
      Top = 24
      Width = 125
      Height = 21
      PasswordChar = '*'
      TabOrder = 0
    end
    object edCert: TEdit
      Left = 15
      Top = 83
      Width = 252
      Height = 21
      Enabled = False
      TabOrder = 1
    end
    object btnBrowseCert: TButton
      Left = 273
      Top = 77
      Width = 75
      Height = 25
      Caption = 'Browse...'
      Enabled = False
      TabOrder = 2
      OnClick = btnBrowseCertClick
    end
    object edCertPassword: TEdit
      Left = 223
      Top = 108
      Width = 125
      Height = 21
      Enabled = False
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object btnEncrypt: TButton
    Left = 105
    Top = 693
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    Default = True
    TabOrder = 7
    OnClick = btnEncryptClick
  end
  object btnCancel: TButton
    Left = 186
    Top = 693
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = btnCancelClick
  end
  object gbGeneralEnc: TGroupBox
    Left = 8
    Top = 154
    Width = 365
    Height = 175
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Encryption properties'
    TabOrder = 4
    object lbEncryptionMethod: TLabel
      Left = 13
      Top = 54
      Width = 94
      Height = 13
      Caption = 'Encryption method:'
    end
    object lbEncryptedDataType: TLabel
      Left = 13
      Top = 30
      Width = 106
      Height = 13
      Caption = 'Encrypted Data Type:'
    end
    object lbMimeType: TLabel
      Left = 12
      Top = 115
      Width = 55
      Height = 13
      Caption = 'Mime Type:'
    end
    object lbExternalFile: TLabel
      Left = 13
      Top = 142
      Width = 63
      Height = 13
      Caption = 'External File:'
    end
    object sbExternalFile: TSpeedButton
      Left = 325
      Top = 138
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = sbExternalFileClick
    end
    object lbXMLNode: TLabel
      Left = 12
      Top = 88
      Width = 51
      Height = 13
      Caption = 'XML Node:'
    end
    object cmbEncryptionMethod: TComboBox
      Left = 223
      Top = 51
      Width = 125
      Height = 22
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        '3DES'
        'AES128'
        'AES192'
        'AES256'
        'Camellia128'
        'Camellia192'
        'Camellia256'
        'DES'
        'RC4'
        'SEED'
        'AES128-GCM'
        'AES192-GCM'
        'AES256-GCM')
    end
    object cmbEncryptedDataType: TComboBox
      Left = 223
      Top = 23
      Width = 125
      Height = 22
      Style = csDropDownList
      TabOrder = 0
      OnChange = cmbEncryptedDataTypeChange
      Items.Strings = (
        'Element'
        'Content'
        'External File')
    end
    object edMimeType: TEdit
      Left = 223
      Top = 112
      Width = 125
      Height = 21
      TabOrder = 3
    end
    object edExternalFile: TEdit
      Left = 175
      Top = 139
      Width = 144
      Height = 21
      TabOrder = 4
    end
    object edXMLNode: TEdit
      Left = 223
      Top = 85
      Width = 125
      Height = 21
      TabOrder = 2
    end
  end
  object gbKEK: TGroupBox
    Left = 8
    Top = 335
    Width = 365
    Height = 201
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Key Encryption Key (KEK) properties'
    TabOrder = 5
    DesignSize = (
      365
      201)
    object lbKeyTransport: TLabel
      Left = 16
      Top = 140
      Width = 72
      Height = 13
      Caption = 'Key Transport:'
    end
    object lbKeyWrap: TLabel
      Left = 16
      Top = 167
      Width = 51
      Height = 13
      Caption = 'Key Wrap:'
    end
    object rgKEK: TRadioGroup
      Left = 15
      Top = 49
      Width = 333
      Height = 68
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Key Encryption Key type:'
      ItemIndex = 1
      Items.Strings = (
        'Key Transport'
        'Key Wrap')
      TabOrder = 1
      OnClick = rgKEKClick
    end
    object cmbKeyTransport: TComboBox
      Left = 223
      Top = 135
      Width = 128
      Height = 22
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'RSA v1.5'
        'RSA-OAEP')
    end
    object cmbKeyWrap: TComboBox
      Left = 223
      Top = 163
      Width = 128
      Height = 22
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        '3DES'
        'AES128'
        'AES192'
        'AES256'
        'Camellia128'
        'Camellia192'
        'Camellia256'
        'SEED')
    end
    object cbEncryptKey: TCheckBox
      Left = 16
      Top = 26
      Width = 97
      Height = 17
      Caption = 'Encrypt Key'
      TabOrder = 0
      OnClick = cbEncryptKeyClick
    end
  end
  object OpenDialogXML: TOpenDialog
    InitialDir = '.'
    Left = 312
    Top = 68
  end
  object SaveDialogXML: TSaveDialog
    InitialDir = '.'
    Left = 312
    Top = 116
  end
  object OpenDialogCert: TOpenDialog
    InitialDir = '.'
    Left = 188
    Top = 612
  end
  object OpenDialogExternal: TOpenDialog
    DefaultExt = '*.*'
    Left = 288
    Top = 256
  end
end


