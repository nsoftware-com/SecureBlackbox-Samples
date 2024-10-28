object FormAuthenticodesigner: TFormAuthenticodesigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Authenticode Signer'
  ClientHeight = 541
  ClientWidth = 550
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
  object lblInfo: TLabel
    Left = 16
    Top = 12
    Width = 432
    Height = 13
    Caption = 
      'This sample illustrates the use of AuthenticodeSigner component ' +
      'to sign EXE and DLL files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 40
    Width = 49
    Height = 13
    Caption = 'Input File:'
  end
  object lblOutputFile: TLabel
    Left = 16
    Top = 71
    Width = 57
    Height = 13
    Caption = 'Output File:'
  end
  object edtInputFile: TEdit
    Left = 79
    Top = 37
    Width = 379
    Height = 21
    TabOrder = 0
  end
  object btnInputBrowse: TButton
    Left = 464
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnInputBrowseClick
  end
  object edtOutputFile: TEdit
    Left = 79
    Top = 68
    Width = 379
    Height = 21
    TabOrder = 2
  end
  object btnOutputBrowse: TButton
    Left = 464
    Top = 66
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btnOutputBrowseClick
  end
  object grpSigningCertificate: TGroupBox
    Left = 8
    Top = 100
    Width = 534
    Height = 93
    Caption = ' Signing Certificate '
    TabOrder = 4
    object lblCertFilename: TLabel
      Left = 16
      Top = 28
      Width = 49
      Height = 13
      Caption = 'File name:'
    end
    object lblCertPassword: TLabel
      Left = 15
      Top = 55
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object btnCertFileBrowse: TButton
      Left = 445
      Top = 23
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 0
      OnClick = btnCertFileBrowseClick
    end
    object edtCertFile: TEdit
      Left = 71
      Top = 25
      Width = 368
      Height = 21
      TabOrder = 1
    end
    object edtCertPassword: TEdit
      Left = 71
      Top = 52
      Width = 346
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object cbxCertShowPassword: TCheckBox
      Left = 423
      Top = 54
      Width = 97
      Height = 17
      Caption = 'Show password'
      TabOrder = 3
      OnClick = cbxCertShowPasswordClick
    end
  end
  object grpSignature: TGroupBox
    Left = 8
    Top = 199
    Width = 534
    Height = 138
    Caption = ' Signature Settings '
    TabOrder = 5
    object lblDescription: TLabel
      Left = 33
      Top = 24
      Width = 57
      Height = 13
      Caption = 'Description:'
    end
    object lblUrl: TLabel
      Left = 67
      Top = 51
      Width = 23
      Height = 13
      Caption = 'URL:'
    end
    object lblHashAlgorithm: TLabel
      Left = 15
      Top = 78
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lblStatement: TLabel
      Left = 302
      Top = 78
      Width = 54
      Height = 13
      Caption = 'Statement:'
    end
    object lblSigningTime: TLabel
      Left = 29
      Top = 105
      Width = 61
      Height = 13
      Caption = 'Signing time:'
    end
    object edtDescription: TEdit
      Left = 96
      Top = 21
      Width = 424
      Height = 21
      TabOrder = 0
    end
    object edtUrl: TEdit
      Left = 96
      Top = 48
      Width = 424
      Height = 21
      TabOrder = 1
    end
    object cmbHashAlgorithm: TComboBox
      Left = 96
      Top = 75
      Width = 160
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 2
      Text = 'SHA-256'
      Items.Strings = (
        'MD5 (legacy)'
        'SHA-1 (legacy)'
        'SHA-224'
        'SHA-256'
        'SHA-384'
        'SHA-512'
        'SHA3-224'
        'SHA3-256'
        'SHA3-384'
        'SHA3-512')
    end
    object rbtIndividual: TRadioButton
      Left = 362
      Top = 77
      Width = 73
      Height = 17
      Caption = 'Individual'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object rbtCommercial: TRadioButton
      Left = 441
      Top = 77
      Width = 79
      Height = 17
      Caption = 'Commercial'
      TabOrder = 4
    end
    object edtSigningTime: TDateTimePicker
      Left = 96
      Top = 102
      Width = 160
      Height = 21
      Date = 43773.594563530100000000
      Time = 43773.594563530100000000
      ShowCheckbox = True
      Checked = False
      Kind = dtkTime
      TabOrder = 5
    end
  end
  object grpTimestamp: TGroupBox
    Left = 8
    Top = 343
    Width = 534
    Height = 159
    Caption = ' Timestamp Settings '
    TabOrder = 6
    object lblTrustedServer: TLabel
      Left = 39
      Top = 76
      Width = 58
      Height = 13
      Caption = 'Server URL:'
    end
    object lblLegacyServer: TLabel
      Left = 39
      Top = 126
      Width = 58
      Height = 13
      Caption = 'Server URL:'
    end
    object rbtNoTimestamp: TRadioButton
      Left = 24
      Top = 27
      Width = 94
      Height = 17
      Caption = 'No timestamp'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbtTrustedClick
    end
    object rbtTrusted: TRadioButton
      Left = 24
      Top = 50
      Width = 181
      Height = 17
      Caption = 'Trusted (according to RFC 3161)'
      TabOrder = 1
      OnClick = rbtTrustedClick
    end
    object cmbTrustedServer: TComboBox
      Left = 103
      Top = 73
      Width = 399
      Height = 21
      TabOrder = 2
      Items.Strings = (
        'http://timestamp.digicert.com'
        'http://tsa.safecreative.org'
        'http://timestamp.geotrust.com/tsa')
    end
    object rbtLegacy: TRadioButton
      Left = 24
      Top = 100
      Width = 139
      Height = 17
      Caption = 'Legacy'
      TabOrder = 3
      OnClick = rbtTrustedClick
    end
    object cmbLegacyServer: TComboBox
      Left = 103
      Top = 123
      Width = 399
      Height = 21
      TabOrder = 4
      Items.Strings = (
        'http://timestamp.verisign.com/scripts/timstamp.dll'
        'http://timestamp.comodoca.com/authenticode'
        'http://timestamp.globalsign.com/scripts/timstamp.dll')
    end
  end
  object btnSign: TButton
    Left = 362
    Top = 508
    Width = 180
    Height = 25
    Caption = 'Sign'
    TabOrder = 7
    OnClick = btnSignClick
  end
  object cbxRemove: TCheckBox
    Left = 8
    Top = 512
    Width = 194
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Remove existing signature(s) if any:'
    TabOrder = 8
  end
  object dlgOpenExe: TOpenDialog
    Filter = 'Executables (*.exe, *.dll)|*.exe;*.dll|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select Input Executable'
    Left = 440
    Top = 326
  end
  object dlgSaveExe: TSaveDialog
    DefaultExt = 'exe'
    Filter = 'Executables (*.exe, *.dll)|*.exe;*.dll|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofEnableSizing]
    Title = 'Select Input Executable'
    Left = 377
    Top = 328
  end
  object dlgOpenCert: TOpenDialog
    Filter = 
      'PEM Certificates (*.pem)|*.pem|PKCS#12 Certificates (*.pfx;*.p12' +
      ')|*.pfx;*.p12|All  supported|*.pem;*.pfx;*.p12'
    FilterIndex = 3
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select Signing Certificate'
    Left = 317
    Top = 319
  end
end


