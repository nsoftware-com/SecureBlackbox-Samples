object FormMessageencryptor: TFormMessageencryptor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Message Encryptor demo'
  ClientHeight = 286
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbInputFile: TLabel
    Left = 8
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 323
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object Label1: TLabel
    Left = 8
    Top = 81
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 323
    Top = 75
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 390
    Height = 13
    Caption = 
      'This sample illustrates how to create encrypted ('#39'enveloped'#39') PK' +
      'CS#7 messages. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
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
    Top = 77
    Width = 245
    Height = 21
    TabOrder = 1
  end
  object btnEncrypt: TButton
    Left = 323
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    TabOrder = 2
    OnClick = btnEncryptClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 110
    Width = 390
    Height = 135
    Caption = 'Encription options  '
    TabOrder = 3
    object lbSymmetricAlgorithm: TLabel
      Left = 8
      Top = 29
      Width = 102
      Height = 13
      Caption = 'Encryption algorithm:'
    end
    object Label2: TLabel
      Left = 8
      Top = 60
      Width = 106
      Height = 13
      Caption = 'Encryption certificate:'
    end
    object Label3: TLabel
      Left = 8
      Top = 109
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object sbCertFile: TSpeedButton
      Left = 310
      Top = 73
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbCertFileClick
    end
    object cmbEncryptionAlgorithm: TComboBox
      Left = 114
      Top = 25
      Width = 175
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        '3DES'
        'RC4'
        'RC2'
        'AES128'
        'AES192'
        'AES256'
        'Twofish128')
    end
    object edCertificate: TEdit
      Left = 8
      Top = 75
      Width = 296
      Height = 21
      TabOrder = 1
    end
    object edCertPassword: TEdit
      Left = 114
      Top = 105
      Width = 190
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
  end
  object dlgOpenFile: TOpenDialog
    Left = 328
    Top = 8
  end
  object dlgSaveFile: TSaveDialog
    Left = 256
    Top = 8
  end
  object DlgOpenCert: TOpenDialog
    Filter = 
      'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*' +
      '.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX'
    Title = 'Select certificate file'
    Left = 344
    Top = 136
  end
end


