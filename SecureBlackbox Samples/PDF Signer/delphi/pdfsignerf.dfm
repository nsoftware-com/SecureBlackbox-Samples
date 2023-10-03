object FormPdfsigner: TFormPdfsigner
  Left = 0
  Top = 0
  Caption = 'PDF Signer demo'
  ClientHeight = 707
  ClientWidth = 728
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
    Width = 636
    Height = 13
    Caption = 
      'This sample illustrates the use of PDFSigner component for signi' +
      'ng PDF documents. Please pick the signing certificate and click ' +
      #39'Sign'#39'. '
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
    Left = 323
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
    Left = 323
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
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
    Top = 80
    Width = 245
    Height = 21
    TabOrder = 1
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 121
    Width = 710
    Height = 175
    Caption = 'Signing options  '
    TabOrder = 2
    object Label3: TLabel
      Left = 10
      Top = 64
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 252
      Top = 81
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label5: TLabel
      Left = 10
      Top = 116
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object Label4: TLabel
      Left = 10
      Top = 29
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object edSigningCertificate: TEdit
      Left = 10
      Top = 83
      Width = 236
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 123
      Top = 112
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object GroupBox5: TGroupBox
      Left = 365
      Top = 64
      Width = 335
      Height = 105
      Caption = 'Signing Chain'
      TabOrder = 2
      object lvSigningCertificates: TListView
        Left = 5
        Top = 24
        Width = 250
        Height = 75
        Columns = <
          item
            Caption = 'Serial'
            Width = 100
          end
          item
            Caption = 'Issuer'
            Width = 145
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemoveCert: TButton
        Left = 260
        Top = 55
        Width = 70
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveCertClick
      end
      object btnAddCert: TButton
        Left = 260
        Top = 24
        Width = 70
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddCertClick
      end
    end
    object cbLevel: TComboBox
      Left = 50
      Top = 25
      Width = 135
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Legacy'
      Items.Strings = (
        'Legacy'
        'BES'
        'EPES'
        'LTV'
        'DocumentTimestamp')
    end
    object cbVisible: TCheckBox
      Left = 225
      Top = 27
      Width = 97
      Height = 17
      Caption = 'Visible signature'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 500
    Width = 710
    Height = 162
    Caption = 'Revocation information '
    TabOrder = 3
    object GroupBox2: TGroupBox
      Left = 10
      Top = 53
      Width = 335
      Height = 105
      Caption = 'Known Certificates'
      TabOrder = 0
      object lvKnownCertificates: TListView
        Left = 5
        Top = 24
        Width = 250
        Height = 75
        Columns = <
          item
            Caption = 'Serial'
            Width = 100
          end
          item
            Caption = 'Issuer'
            Width = 140
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemoveKnown: TButton
        Left = 260
        Top = 55
        Width = 70
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveKnownClick
      end
      object btnAddKnown: TButton
        Left = 260
        Top = 24
        Width = 70
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddKnownClick
      end
    end
    object GroupBox4: TGroupBox
      Left = 365
      Top = 53
      Width = 335
      Height = 105
      Caption = 'Trusted Certificates'
      TabOrder = 1
      object lvTrustedCertificates: TListView
        Left = 5
        Top = 24
        Width = 250
        Height = 75
        Columns = <
          item
            Caption = 'Serial'
            Width = 100
          end
          item
            Caption = 'Issuer'
            Width = 140
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemoveTrusted: TButton
        Left = 260
        Top = 55
        Width = 70
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveTrustedClick
      end
      object btnAddTrusted: TButton
        Left = 260
        Top = 24
        Width = 70
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddTrustedClick
      end
    end
    object cbIncludeLocalRevInfo: TCheckBox
      Left = 10
      Top = 23
      Width = 270
      Height = 17
      Caption = 'Include local revocation information to the signature'
      TabOrder = 2
    end
    object cbIncludeRevocationInfoToAdbeAttribute: TCheckBox
      Left = 315
      Top = 23
      Width = 270
      Height = 17
      Caption = 'Include revocationInfo to Adbe attribute'
      TabOrder = 3
    end
  end
  object btnSign: TButton
    Left = 648
    Top = 674
    Width = 70
    Height = 25
    Caption = 'Sign'
    TabOrder = 4
    OnClick = btnSignClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 300
    Width = 710
    Height = 195
    Caption = 'Additional options  '
    TabOrder = 5
    object lAuthorName: TLabel
      Left = 10
      Top = 23
      Width = 73
      Height = 13
      Caption = 'Author'#39's name:'
    end
    object lReason: TLabel
      Left = 10
      Top = 63
      Width = 93
      Height = 13
      Caption = 'Reason for signing:'
    end
    object editAuthorName: TEdit
      Left = 10
      Top = 38
      Width = 249
      Height = 21
      TabOrder = 0
    end
    object cbReason: TComboBox
      Left = 10
      Top = 78
      Width = 290
      Height = 21
      TabOrder = 1
      Text = '<none>'
      Items.Strings = (
        'I am the author of this document'
        
          'I agree to the terms defined by placement of my signature on thi' +
          's document'
        'I have reviewed this document'
        'I attest to the accuracy and integrity of this document'
        'I am approving this document')
    end
    object cbAutoCollectRevInfo: TCheckBox
      Left = 10
      Top = 117
      Width = 293
      Height = 17
      Caption = 'Automatically collect missing revocation information'
      TabOrder = 2
    end
    object cbIgnoreChainValidationErrors: TCheckBox
      Left = 10
      Top = 144
      Width = 261
      Height = 17
      Caption = 'Ignore chain validation errors'
      TabOrder = 3
    end
    object cbForceCompleteChainValidation: TCheckBox
      Left = 10
      Top = 171
      Width = 171
      Height = 17
      Caption = 'Force complete chain validation'
      TabOrder = 4
    end
    object cbDeepValidation: TCheckBox
      Left = 211
      Top = 171
      Width = 96
      Height = 17
      Caption = 'Deep validation'
      TabOrder = 5
    end
    object editTSPServer: TEdit
      Left = 384
      Top = 169
      Width = 296
      Height = 21
      TabOrder = 6
      Text = 'http://'
    end
    object cbRequestTimestamp: TCheckBox
      Left = 365
      Top = 144
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 7
    end
    object GroupBox7: TGroupBox
      Left = 365
      Top = 23
      Width = 335
      Height = 110
      Caption = 'Policy  '
      TabOrder = 8
      object Label6: TLabel
        Left = 10
        Top = 54
        Width = 75
        Height = 13
        Caption = 'Hash algorithm:'
      end
      object Label7: TLabel
        Left = 10
        Top = 27
        Width = 48
        Height = 13
        Caption = 'Identifier:'
      end
      object Label11: TLabel
        Left = 10
        Top = 81
        Width = 57
        Height = 13
        Caption = 'Hash value:'
      end
      object cbHashAlgorithm: TComboBox
        Left = 95
        Top = 50
        Width = 138
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Items.Strings = (
          ''
          'SHA1'
          'MD5'
          'SHA256'
          'SHA384'
          'SHA512'
          'RIPEMD160')
      end
      object edIdentifier: TEdit
        Left = 95
        Top = 20
        Width = 235
        Height = 21
        TabOrder = 1
      end
      object edHashValue: TEdit
        Left = 95
        Top = 80
        Width = 235
        Height = 21
        TabOrder = 2
      end
    end
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


