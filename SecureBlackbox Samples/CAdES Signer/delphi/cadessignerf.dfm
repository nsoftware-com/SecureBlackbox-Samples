object FormCadessigner: TFormCadessigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CAdES Signer demo'
  ClientHeight = 328
  ClientWidth = 694
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
    Left = 18
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 355
    Top = 43
    Width = 70
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object Label1: TLabel
    Left = 10
    Top = 84
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 355
    Top = 78
    Width = 70
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 589
    Height = 13
    Caption = 
      'This sample shows how to create CAdES signatures. Please select ' +
      'an input file, tune up the signing options, and click '#39'Sign'#39'.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputFile: TEdit
    Left = 69
    Top = 45
    Width = 280
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 81
    Width = 280
    Height = 21
    TabOrder = 1
  end
  object btnSign: TButton
    Left = 613
    Top = 295
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 120
    Width = 680
    Height = 161
    Caption = 'Signing options  '
    TabOrder = 2
    object Label4: TLabel
      Left = 481
      Top = 26
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object Label2: TLabel
      Left = 10
      Top = 25
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 347
      Top = 20
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label3: TLabel
      Left = 49
      Top = 53
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object lbHashAlgorithm: TLabel
      Left = 435
      Top = 54
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 22
      Width = 236
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 105
      Top = 49
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object cbLevel: TComboBox
      Left = 516
      Top = 22
      Width = 148
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object cmbHashAlgorithm: TComboBox
      Left = 516
      Top = 49
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
    object cbDetached: TCheckBox
      Left = 516
      Top = 76
      Width = 121
      Height = 17
      Caption = 'Detached'
      TabOrder = 4
    end
    object cbRequestTimestamp: TCheckBox
      Left = 10
      Top = 103
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 5
    end
    object editTSPServer: TEdit
      Left = 19
      Top = 126
      Width = 296
      Height = 21
      TabOrder = 6
      Text = 'http://'
    end
  end
  object dlgOpenFile: TOpenDialog
    Left = 568
    Top = 48
  end
  object dlgSaveFile: TSaveDialog
    Left = 480
    Top = 48
  end
  object DlgOpenCert: TOpenDialog
    Filter = 
      'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*' +
      '.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX'
    Title = 'Select certificate file'
    Left = 648
    Top = 48
  end
end


