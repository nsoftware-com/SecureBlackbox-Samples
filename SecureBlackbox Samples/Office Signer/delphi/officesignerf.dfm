object FormOfficesigner: TFormOfficesigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Office Signer demo'
  ClientHeight = 343
  ClientWidth = 726
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
    Width = 401
    Height = 13
    Caption = 
      'This sample illustrates the use of OfficeSigner component to sig' +
      'n office documents. '
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
  object sbBrowseInputFile: TSpeedButton
    Left = 355
    Top = 43
    Width = 75
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
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 279
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 80
    Width = 279
    Height = 21
    TabOrder = 1
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 115
    Width = 710
    Height = 185
    Caption = 'Signing options  '
    TabOrder = 2
    object Label3: TLabel
      Left = 10
      Top = 28
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 347
      Top = 23
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label5: TLabel
      Left = 49
      Top = 56
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label4: TLabel
      Left = 430
      Top = 29
      Width = 77
      Height = 13
      Caption = 'Signature Type:'
    end
    object Label2: TLabel
      Left = 431
      Top = 55
      Width = 76
      Height = 13
      Caption = 'Hash Algorithm:'
    end
    object Label7: TLabel
      Left = 432
      Top = 126
      Width = 75
      Height = 13
      Caption = 'Signature Text:'
    end
    object Label11: TLabel
      Left = 404
      Top = 156
      Width = 103
      Height = 13
      Caption = 'Signature Comments:'
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 25
      Width = 236
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 104
      Top = 52
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object cbSignatureType: TComboBox
      Left = 513
      Top = 25
      Width = 190
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'Default'
      Items.Strings = (
        'Default'
        'BinaryCryptoAPI'
        'BinaryXML'
        'OpenXML'
        'OpenXPS'
        'OpenOffice')
    end
    object cbHashAlgorithm: TComboBox
      Left = 513
      Top = 52
      Width = 190
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 6
      Text = 'SHA256'
      Items.Strings = (
        'MD5'
        'SHA1'
        'SHA224'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160'
        'GOST'
        'Whirlpool'
        'SHA3_224'
        'SHA3_256'
        'SHA3_384'
        'SHA3_512')
    end
    object cbSignDocument: TCheckBox
      Left = 10
      Top = 100
      Width = 293
      Height = 17
      Caption = 'Sign Document'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbSignSignatureOrigin: TCheckBox
      Left = 10
      Top = 125
      Width = 261
      Height = 17
      Caption = 'Sign Signature Origin'
      TabOrder = 4
    end
    object cbSignCoreProperties: TCheckBox
      Left = 10
      Top = 150
      Width = 171
      Height = 17
      Caption = 'Sign Core Properties'
      TabOrder = 3
    end
    object cbIncludeSigInfo: TCheckBox
      Left = 513
      Top = 100
      Width = 143
      Height = 17
      Caption = '  Include signature info'
      TabOrder = 7
    end
    object edSigText: TEdit
      Left = 513
      Top = 122
      Width = 190
      Height = 21
      TabOrder = 8
    end
    object edSigComments: TEdit
      Left = 513
      Top = 152
      Width = 190
      Height = 21
      TabOrder = 9
    end
  end
  object btnSign: TButton
    Left = 648
    Top = 310
    Width = 70
    Height = 25
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
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


