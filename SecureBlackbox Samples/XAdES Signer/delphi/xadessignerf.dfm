object FormXadessigner: TFormXadessigner
  Left = 227
  Top = 151
  Width = 713
  Height = 535
  Caption = 'XAdES Signer demo'
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
  object lbXMLFile: TLabel
    Left = 8
    Top = 51
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseXMLFile: TSpeedButton
    Left = 370
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
    Left = 370
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
    Width = 295
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 77
    Width = 295
    Height = 21
    TabOrder = 1
  end
  object btnReferences: TButton
    Left = 13
    Top = 458
    Width = 75
    Height = 25
    Caption = 'References'
    TabOrder = 2
    OnClick = btnReferencesClick
  end
  object btnSign: TButton
    Left = 606
    Top = 458
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
  end
  object GroupBox1: TGroupBox
    Left = 362
    Top = 113
    Width = 312
    Height = 160
    Caption = 'XAdES options  '
    TabOrder = 4
    object lbVersion: TLabel
      Left = 23
      Top = 49
      Width = 39
      Height = 13
      Caption = 'Version:'
    end
    object Label2: TLabel
      Left = 33
      Top = 76
      Width = 28
      Height = 13
      Caption = 'Form:'
    end
    object cmbVersion: TComboBox
      Left = 68
      Top = 46
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        '1.1.1'
        '1.2.2'
        '1.3.2'
        '1.4.1')
    end
    object cbTimestamp: TCheckBox
      Left = 8
      Top = 100
      Width = 207
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 1
    end
    object edTimestampServer: TEdit
      Left = 24
      Top = 120
      Width = 273
      Height = 21
      TabOrder = 2
      Text = 'http://'
    end
    object cmbForm: TComboBox
      Left = 67
      Top = 73
      Width = 142
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        'XAdES'
        'XAdES-BES'
        'XAdES-EPES'
        'XAdES-T'
        'XAdES-C'
        'XAdES-X'
        'XAdES-X-L'
        'XAdES-A'
        'XAdES-E-BES'
        'XAdES-E-EPES'
        'XAdES-E-T'
        'XAdES-E-C'
        'XAdES-E-X'
        'XAdES-E-X-Long'
        'XAdES-E-X-L'
        'XAdES-E-A')
    end
    object cbEnableXAdES: TCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Enable XAdES'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object gbGeneralEnc: TGroupBox
    Left = 8
    Top = 115
    Width = 348
    Height = 158
    Caption = 'General'
    TabOrder = 5
    DesignSize = (
      348
      158)
    object lbHashAlgorithm: TLabel
      Left = 16
      Top = 90
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lbCanonMethod: TLabel
      Left = 16
      Top = 50
      Width = 77
      Height = 26
      Caption = 'Canonicalization'#13#10'method:'
    end
    object lbSignatureLocation: TLabel
      Left = 16
      Top = 113
      Width = 93
      Height = 13
      Caption = 'Signature Location:'
    end
    object cmbHashAlgorithm: TComboBox
      Left = 144
      Top = 86
      Width = 184
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'SHA1'
        'MD5'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160')
    end
    object cmbCanonMethod: TComboBox
      Left = 144
      Top = 53
      Width = 184
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'Canonical'
        'Canonical with comments'
        'Canonical v1.1'
        'Canonical with comments v1.1'
        'Exclusive canonical'
        'Exclusive canonical with comments'
        'Minimal canonical')
    end
    object cbDetached: TCheckBox
      Left = 16
      Top = 26
      Width = 97
      Height = 17
      Caption = 'Detached'
      TabOrder = 2
    end
    object edSignatureLocation: TEdit
      Left = 16
      Top = 129
      Width = 313
      Height = 21
      TabOrder = 3
    end
  end
  object gbKeyInfo: TGroupBox
    Left = 8
    Top = 277
    Width = 673
    Height = 170
    Caption = 'Key Info'
    TabOrder = 6
    DesignSize = (
      673
      170)
    object lbKeyName: TLabel
      Left = 16
      Top = 52
      Width = 52
      Height = 13
      Caption = 'Key Name:'
    end
    object Label5: TLabel
      Left = 16
      Top = 132
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object Label3: TLabel
      Left = 16
      Top = 80
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 258
      Top = 97
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object edKeyName: TEdit
      Left = 80
      Top = 48
      Width = 252
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object cbIncludeKey: TCheckBox
      Left = 16
      Top = 24
      Width = 209
      Height = 17
      Caption = 'Include Key (public part)'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object gbSigningCertificates: TGroupBox
      Left = 358
      Top = 19
      Width = 307
      Height = 134
      Caption = 'Signing Chain'
      TabOrder = 4
      object lvSigningCertificates: TListView
        Left = 10
        Top = 23
        Width = 207
        Height = 98
        Columns = <
          item
            Caption = 'Serial'
            Width = 70
          end
          item
            Caption = 'Issuer'
            Width = 130
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemove: TButton
        Left = 224
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveClick
      end
      object btnAdd: TButton
        Left = 224
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddClick
      end
    end
    object edCertPassword: TEdit
      Left = 129
      Top = 128
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object edSigningCertificate: TEdit
      Left = 16
      Top = 99
      Width = 236
      Height = 21
      TabOrder = 2
    end
  end
  object dlgOpenXML: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 392
    Top = 308
  end
  object dlgSaveXML: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 456
    Top = 316
  end
  object DlgOpen: TOpenDialog
    Left = 392
    Top = 348
  end
  object FSigner: TsbxXAdESSigner
    Left = 520
    Top = 40
  end
end


