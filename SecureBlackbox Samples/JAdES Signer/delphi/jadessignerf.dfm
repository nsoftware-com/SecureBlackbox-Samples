object FormJadessigner: TFormJadessigner
  Left = 0
  Top = 0
  Caption = 'JAdES Signer demo'
  ClientHeight = 691
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
    Top = 8
    Width = 682
    Height = 13
    Caption = 
      'This sample illustrates the use of JAdESSigner component for cre' +
      'ating JWS/JAdES signature. Please pick the signing certificate a' +
      'nd click '#39'Sign'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lbPayload: TLabel
    Left = 8
    Top = 32
    Width = 94
    Height = 13
    Caption = 'Payload (as string):'
  end
  object Label1: TLabel
    Left = 10
    Top = 118
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 398
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object edOutputFile: TEdit
    Left = 71
    Top = 115
    Width = 321
    Height = 21
    TabOrder = 0
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 148
    Width = 710
    Height = 175
    Caption = 'Signing options  '
    TabOrder = 1
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
      Left = 45
      Top = 25
      Width = 135
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 3
      Text = 'JAdES Baseline-B'
      Items.Strings = (
        'JSON Web Signature'
        'JAdES Baseline-B'
        'JAdES Baseline-T'
        'JAdES Baseline-LT'
        'JAdES Baseline-LTA')
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 481
    Width = 710
    Height = 162
    Caption = 'Revocation information '
    TabOrder = 2
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
  end
  object btnSign: TButton
    Left = 648
    Top = 655
    Width = 70
    Height = 25
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 327
    Width = 710
    Height = 150
    Caption = 'Additional options  '
    TabOrder = 4
    object cbIgnoreChainValidationErrors: TCheckBox
      Left = 365
      Top = 90
      Width = 261
      Height = 17
      Caption = 'Ignore chain validation errors'
      TabOrder = 0
    end
    object cbForceCompleteChainValidation: TCheckBox
      Left = 365
      Top = 113
      Width = 171
      Height = 17
      Caption = 'Force complete chain validation'
      TabOrder = 1
    end
    object editTSPServer: TEdit
      Left = 384
      Top = 55
      Width = 296
      Height = 21
      TabOrder = 2
      Text = 'http://'
    end
    object cbRequestTimestamp: TCheckBox
      Left = 365
      Top = 30
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 3
    end
    object GroupBox7: TGroupBox
      Left = 10
      Top = 27
      Width = 335
      Height = 110
      Caption = 'Policy  '
      TabOrder = 4
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
          'SHA256'
          'SHA384'
          'SHA512')
      end
      object edIdentifier: TEdit
        Left = 95
        Top = 23
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
  object cbCompactForm: TCheckBox
    Left = 532
    Top = 94
    Width = 97
    Height = 17
    Caption = 'Compact Form'
    TabOrder = 5
  end
  object mmPayload: TMemo
    Left = 8
    Top = 51
    Width = 465
    Height = 54
    Lines.Strings = (
      'Hello, world!')
    TabOrder = 6
  end
  object cbFlattenedSignature: TCheckBox
    Left = 532
    Top = 117
    Width = 134
    Height = 17
    Caption = 'Flattened Signature'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object cbDetached: TCheckBox
    Left = 532
    Top = 53
    Width = 97
    Height = 17
    Caption = 'Detached'
    TabOrder = 8
  end
  object dlgOpen: TOpenDialog
    Left = 160
    Top = 48
  end
  object dlgSave: TSaveDialog
    Left = 208
    Top = 48
  end
end


