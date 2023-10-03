object FormPgpwriter: TFormPgpwriter
  Left = 280
  Top = 120
  BorderStyle = bsDialog
  Caption = 'PGP Writer demo'
  ClientHeight = 329
  ClientWidth = 533
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
  object lbInputFileName: TLabel
    Left = 8
    Top = 49
    Width = 76
    Height = 13
    Caption = 'Files to protect:'
  end
  object Label1: TLabel
    Left = 9
    Top = 84
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 494
    Height = 13
    Caption = 
      'This sample showcases the use of PGPWriter to create encrypted a' +
      'nd signed OpenPGP-compliant files. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnBrowseInputFile: TButton
    Left = 448
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 0
    OnClick = btnBrowseInputFileClick
  end
  object btnEncrypt: TButton
    Left = 416
    Top = 294
    Width = 107
    Height = 25
    Caption = 'Encrypt and sign'
    TabOrder = 3
    OnClick = btnEncryptClick
  end
  object editOutputFile: TEdit
    Left = 95
    Top = 80
    Width = 337
    Height = 21
    TabOrder = 1
  end
  object btnBrowseOutFile: TButton
    Left = 448
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 2
    OnClick = btnBrowseOutputFileClick
  end
  object editInputFile: TEdit
    Left = 95
    Top = 45
    Width = 337
    Height = 21
    TabOrder = 4
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 120
    Width = 515
    Height = 161
    Caption = 'Keys  '
    TabOrder = 5
    object lbPublicKeyList: TLabel
      Left = 10
      Top = 109
      Width = 141
      Height = 13
      Caption = 'Please select the signing key:'
    end
    object lbSecretKeyList: TLabel
      Left = 10
      Top = 64
      Width = 159
      Height = 13
      Caption = 'Please select the encryption key:'
    end
    object cbEncryptKeySelect: TComboBox
      Left = 10
      Top = 80
      Width = 495
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object cbSignKeySelect: TComboBox
      Left = 10
      Top = 128
      Width = 495
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object btnBrowseKeyringFile: TButton
      Left = 367
      Top = 33
      Width = 138
      Height = 25
      Caption = 'Browse for keyring files ...'
      TabOrder = 2
      OnClick = btnBrowseKeyringFileClick
    end
  end
  object dlgOpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 169
    Top = 136
  end
  object dlgSaveDialog: TSaveDialog
    Left = 249
    Top = 136
  end
end


