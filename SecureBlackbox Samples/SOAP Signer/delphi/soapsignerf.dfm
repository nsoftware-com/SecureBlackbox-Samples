object FormSoapsigner: TFormSoapsigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SOAP Signer demo'
  ClientHeight = 327
  ClientWidth = 697
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
  object lInputFile: TLabel
    Left = 16
    Top = 51
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 383
    Top = 45
    Width = 75
    Height = 25
    Caption = 'Browse...'
    OnClick = sbBrowseInputFileClick
  end
  object lOutputFile: TLabel
    Left = 8
    Top = 81
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbBrowseOutputFile: TSpeedButton
    Left = 383
    Top = 75
    Width = 75
    Height = 25
    Caption = 'Browse...'
    OnClick = sbBrowseOutputFileClick
  end
  object lDemoInfo: TLabel
    Left = 8
    Top = 15
    Width = 625
    Height = 13
    Caption = 
      'This sample shows how to create SOAP or WSS signatures. Please s' +
      'elect an input file, tune up the signing options, and click '#39'Sig' +
      'n'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputFile: TEdit
    Left = 69
    Top = 47
    Width = 308
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 77
    Width = 308
    Height = 21
    TabOrder = 1
  end
  object btnReferences: TButton
    Left = 8
    Top = 290
    Width = 129
    Height = 25
    Caption = 'Additional References'
    TabOrder = 3
    OnClick = btnReferencesClick
  end
  object btnSign: TButton
    Left = 614
    Top = 290
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 4
    OnClick = btnSignClick
  end
  object gbSigningOptions: TGroupBox
    Left = 8
    Top = 110
    Width = 680
    Height = 169
    Caption = 'Signing options  '
    TabOrder = 2
    DesignSize = (
      680
      169)
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
      Left = 375
      Top = 17
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object lCertEmbed: TLabel
      Left = 347
      Top = 93
      Width = 122
      Height = 13
      Caption = 'Embed Certificate option:'
    end
    object lKeyName: TLabel
      Left = 417
      Top = 120
      Width = 52
      Height = 13
      Caption = 'Key Name:'
    end
    object lSignatureType: TLabel
      Left = 22
      Top = 92
      Width = 77
      Height = 13
      Caption = 'Signature Type:'
    end
    object lHashAlgorithm: TLabel
      Left = 24
      Top = 119
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object edCertPassword: TEdit
      Left = 105
      Top = 46
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 19
      Width = 264
      Height = 21
      TabOrder = 0
    end
    object cbIncludeKey: TCheckBox
      Left = 475
      Top = 91
      Width = 160
      Height = 17
      Caption = 'Include Key (public part)'
      Checked = True
      State = cbChecked
      TabOrder = 5
      Visible = False
    end
    object cmbCertEmbed: TComboBox
      Left = 475
      Top = 89
      Width = 198
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 6
      Text = 'In Binary Security Token'
      Items.Strings = (
        'In Signature'
        'In Binary Security Token'
        'In Signed Binary Security Token,'
        'In Binary Security Token And Signature'
        'None')
    end
    object edKeyName: TEdit
      Left = 475
      Top = 116
      Width = 198
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
    end
    object cmbSignatureType: TComboBox
      Left = 105
      Top = 89
      Width = 176
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 2
      Text = 'WSS Signature'
      OnChange = cmbSignatureTypeChange
      Items.Strings = (
        'WSS Signature'
        'SOAP Signature ')
    end
    object cbSignBody: TCheckBox
      Left = 105
      Top = 143
      Width = 97
      Height = 17
      Caption = 'Sign Body'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object cmbHashAlgorithm: TComboBox
      Left = 105
      Top = 116
      Width = 176
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
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
  object dlgOpenSOAP: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 568
    Top = 56
  end
  object dlgSaveSOAP: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 496
    Top = 56
  end
  object DlgOpen: TOpenDialog
    Left = 632
    Top = 56
  end
end


