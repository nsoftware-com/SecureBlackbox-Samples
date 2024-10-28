object FormMessagesigner: TFormMessagesigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Message Signer demo'
  ClientHeight = 316
  ClientWidth = 513
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
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 488
    Height = 13
    Caption = 
      'This sample illustrates the use of MessageSigner component to cr' +
      'eate PKCS#7-compliant signed files. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lbInputFile: TLabel
    Left = 18
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 418
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object Label1: TLabel
    Left = 10
    Top = 76
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 418
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object btnSign: TButton
    Left = 428
    Top = 283
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 0
    OnClick = btnSignClick
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 105
    Width = 495
    Height = 168
    Caption = 'Signing options  '
    TabOrder = 1
    object Label2: TLabel
      Left = 10
      Top = 27
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 410
      Top = 22
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label3: TLabel
      Left = 49
      Top = 55
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object lbSignatureType: TLabel
      Left = 24
      Top = 99
      Width = 75
      Height = 13
      Caption = 'Signature type:'
    end
    object lbHashAlgorithm: TLabel
      Left = 24
      Top = 127
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 24
      Width = 299
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 105
      Top = 51
      Width = 192
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object cmbSignatureType: TComboBox
      Left = 105
      Top = 96
      Width = 189
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'PKCS1 Detached'
        'PKCS7 Detached'
        'PKCS7 Enveloping')
    end
    object cmbHashAlgorithm: TComboBox
      Left = 105
      Top = 123
      Width = 149
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'SHA1'
        'MD5'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160')
    end
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 342
    Height = 21
    TabOrder = 2
  end
  object edOutputFile: TEdit
    Left = 71
    Top = 72
    Width = 341
    Height = 21
    TabOrder = 3
  end
  object dlgOpenFile: TOpenDialog
    Left = 440
    Top = 168
  end
  object dlgSaveFile: TSaveDialog
    Left = 360
    Top = 168
  end
  object DlgOpenCert: TOpenDialog
    Filter = 
      'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*' +
      '.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX'
    Title = 'Select certificate file'
    Left = 440
    Top = 224
  end
end


