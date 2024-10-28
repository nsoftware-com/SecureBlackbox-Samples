object FormXadessigner: TFormXadessigner
  Left = 227
  Top = 151
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'XAdES Signer demo'
  ClientHeight = 358
  ClientWidth = 634
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
  object lbXMLFile: TLabel
    Left = 8
    Top = 51
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseXMLFile: TSpeedButton
    Left = 358
    Top = 45
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseXMLFileClick
  end
  object Label1: TLabel
    Left = 8
    Top = 81
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 358
    Top = 75
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 591
    Height = 13
    Caption = 
      'This sample shows how to create XAdES signatures. Please select ' +
      'an input file, tune up the signing options, and click '#39'Sign'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edXMLFile: TEdit
    Left = 69
    Top = 47
    Width = 283
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 77
    Width = 283
    Height = 21
    TabOrder = 1
  end
  object btnReferences: TButton
    Left = 8
    Top = 325
    Width = 75
    Height = 25
    Caption = 'References'
    TabOrder = 3
    OnClick = btnReferencesClick
  end
  object btnSign: TButton
    Left = 553
    Top = 325
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 4
    OnClick = btnSignClick
  end
  object gbSigningOptions: TGroupBox
    Left = 8
    Top = 106
    Width = 620
    Height = 207
    Caption = 'Signing options  '
    TabOrder = 2
    DesignSize = (
      620
      207)
    object lbKeyName: TLabel
      Left = 47
      Top = 107
      Width = 52
      Height = 13
      Caption = 'Key Name:'
    end
    object Label5: TLabel
      Left = 49
      Top = 50
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label3: TLabel
      Left = 10
      Top = 23
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 350
      Top = 17
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object lbVersion: TLabel
      Left = 434
      Top = 22
      Width = 39
      Height = 13
      Caption = 'Version:'
    end
    object Label2: TLabel
      Left = 441
      Top = 50
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object lbCanonMethod: TLabel
      Left = 388
      Top = 103
      Width = 81
      Height = 13
      Caption = 'Canonicalization:'
    end
    object lbHashAlgorithm: TLabel
      Left = 394
      Top = 76
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lbSignatureLocation: TLabel
      Left = 22
      Top = 174
      Width = 77
      Height = 13
      Caption = 'Signature node:'
    end
    object edKeyName: TEdit
      Left = 105
      Top = 103
      Width = 185
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object cbIncludeKey: TCheckBox
      Left = 10
      Top = 80
      Width = 209
      Height = 17
      Caption = 'Include Key (public part)'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object edCertPassword: TEdit
      Left = 105
      Top = 46
      Width = 185
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 19
      Width = 239
      Height = 21
      TabOrder = 0
    end
    object cmbVersion: TComboBox
      Left = 475
      Top = 19
      Width = 134
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 6
      Text = '1.3.2'
      Items.Strings = (
        '1.1.1'
        '1.2.2'
        '1.3.2'
        '1.4.1')
    end
    object cmbLevel: TComboBox
      Left = 475
      Top = 46
      Width = 134
      Height = 21
      Style = csDropDownList
      ItemIndex = 4
      TabOrder = 7
      Text = 'BES'
      Items.Strings = (
        'BaselineB'
        'BaselineT'
        'BaselineLT'
        'BaselineLTA'
        'BES'
        'EPES'
        'T'
        'C'
        'X'
        'XL'
        'A'
        'ExtendedBES'
        'ExtendedEPES'
        'ExtendedT'
        'ExtendedC'
        'ExtendedX'
        'ExtendedXLong'
        'ExtendedXL'
        'ExtendedA'
        'Generic (XML-DSIG)')
    end
    object cbTimestamp: TCheckBox
      Left = 333
      Top = 148
      Width = 207
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 10
    end
    object edTimestampServer: TEdit
      Left = 344
      Top = 171
      Width = 265
      Height = 21
      TabOrder = 11
      Text = 'http://'
    end
    object cbDetached: TCheckBox
      Left = 10
      Top = 148
      Width = 159
      Height = 17
      Caption = 'Create detached signature'
      TabOrder = 4
    end
    object cmbCanonMethod: TComboBox
      Left = 475
      Top = 100
      Width = 134
      Height = 21
      Style = csDropDownList
      TabOrder = 9
      Items.Strings = (
        'Canonical'
        'Canonical with comments'
        'Canonical v1.1'
        'Canonical with comments v1.1'
        'Exclusive canonical'
        'Exclusive canonical with comments'
        'Minimal canonical')
    end
    object cmbHashAlgorithm: TComboBox
      Left = 475
      Top = 73
      Width = 134
      Height = 21
      Style = csDropDownList
      TabOrder = 8
      Items.Strings = (
        'SHA1'
        'MD5'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160')
    end
    object edSignatureNode: TEdit
      Left = 105
      Top = 171
      Width = 185
      Height = 21
      TabOrder = 5
    end
  end
  object dlgOpenXML: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 472
    Top = 44
  end
  object dlgSaveXML: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 528
    Top = 44
  end
  object DlgOpen: TOpenDialog
    Left = 584
    Top = 44
  end
end


