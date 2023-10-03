object FormPdfencryptor: TFormPdfencryptor
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'PDF Encryptor demo'
  ClientHeight = 484
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lSourceFile: TLabel
    Left = 8
    Top = 60
    Width = 76
    Height = 13
    Caption = 'Source PDF file:'
  end
  object lDestFile: TLabel
    Left = 8
    Top = 108
    Width = 97
    Height = 13
    Caption = 'Destination PDF file:'
  end
  object lDemoInfo: TLabel
    Left = 8
    Top = 8
    Width = 355
    Height = 46
    AutoSize = False
    Caption = 
      'This sample illustrates the use of PDFEncryptor component for en' +
      'crypting PDF documents. Please pick the encryption type and spec' +
      'ify either encryption password or certificate and then click '#39'En' +
      'crypt'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object editSource: TEdit
    Left = 8
    Top = 79
    Width = 281
    Height = 21
    TabOrder = 0
  end
  object editDest: TEdit
    Left = 8
    Top = 124
    Width = 281
    Height = 21
    TabOrder = 2
  end
  object btnBrowseSource: TButton
    Left = 295
    Top = 78
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
    Top = 154
    Width = 361
    Height = 281
    Caption = 'Encryption properties'
    TabOrder = 4
    object lCertificate: TLabel
      Left = 40
      Top = 176
      Width = 106
      Height = 13
      Caption = 'Encryption certificate:'
      Enabled = False
    end
    object lCertPassword: TLabel
      Left = 40
      Top = 224
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
      Enabled = False
    end
    object lEncryptionAlgorithm: TLabel
      Left = 16
      Top = 24
      Width = 102
      Height = 13
      Caption = 'Encryption algorithm:'
    end
    object lPassword: TLabel
      Left = 40
      Top = 104
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object rbPasswordEncryption: TRadioButton
      Left = 16
      Top = 81
      Width = 137
      Height = 17
      Caption = 'Password encryption'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rbPasswordEncryptionClick
    end
    object rbPublicKeyEncryption: TRadioButton
      Left = 18
      Top = 153
      Width = 161
      Height = 17
      Caption = 'Public key encryption'
      TabOrder = 4
      OnClick = rbPublicKeyEncryptionClick
    end
    object editPassword: TEdit
      Left = 41
      Top = 120
      Width = 129
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object editCert: TEdit
      Left = 41
      Top = 195
      Width = 225
      Height = 21
      Enabled = False
      TabOrder = 5
    end
    object btnBrowseCert: TButton
      Left = 272
      Top = 190
      Width = 75
      Height = 25
      Caption = 'Browse...'
      Enabled = False
      TabOrder = 6
      OnClick = btnBrowseCertClick
    end
    object editCertPassword: TEdit
      Left = 41
      Top = 243
      Width = 129
      Height = 21
      Enabled = False
      PasswordChar = '*'
      TabOrder = 7
    end
    object cbAlgorithm: TComboBox
      Left = 16
      Top = 43
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'RC4/40 bits (Acrobat 4)'
        'RC4/128 bits (Acrobat 5)'
        'AES/128 bits (Acrobat 6, 7)'
        'AES/256 bits (Acrobat 9)'
        'AES/256 bits (Acrobat X)')
    end
    object cbEncryptMetadata: TCheckBox
      Left = 184
      Top = 46
      Width = 161
      Height = 17
      Caption = 'Encrypt document metadata'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object btnEncrypt: TButton
    Left = 112
    Top = 448
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    Default = True
    TabOrder = 5
    OnClick = btnEncryptClick
  end
  object btnCancel: TButton
    Left = 192
    Top = 448
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = btnCancelClick
  end
  object OpenDialogPDF: TOpenDialog
    InitialDir = '.'
    Left = 312
    Top = 68
  end
  object SaveDialogPDF: TSaveDialog
    InitialDir = '.'
    Left = 312
    Top = 116
  end
  object OpenDialogCert: TOpenDialog
    InitialDir = '.'
    Left = 304
    Top = 348
  end
end


