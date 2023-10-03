object FormAsicsigner: TFormAsicsigner
  Left = 0
  Top = 0
  Caption = 'ASiC Signer demo'
  ClientHeight = 692
  ClientWidth = 727
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
  DesignSize = (
    727
    692)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 150
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 355
    Top = 145
    Width = 70
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object lbInputFileName: TLabel
    Left = 8
    Top = 40
    Width = 133
    Height = 13
    Caption = 'Input files list (files to sign):'
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 710
    Height = 13
    Caption = 
      'This sample shows how to create ASiC signatures. Please select i' +
      'nput files, a signing certificate, and the desired ASiC type and' +
      ' level, and click '#39'Sign'#39'.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnSign: TButton
    Left = 648
    Top = 655
    Width = 70
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Sign'
    TabOrder = 0
    OnClick = btnSignClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 512
    Width = 710
    Height = 129
    Caption = 'Revocation information '
    TabOrder = 1
    object GroupBox2: TGroupBox
      Left = 10
      Top = 17
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
      Top = 18
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
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 147
    Width = 275
    Height = 21
    TabOrder = 2
  end
  object bbClear: TButton
    Left = 355
    Top = 57
    Width = 70
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = bbClearClick
  end
  object btnBrowseInputFile: TButton
    Left = 355
    Top = 87
    Width = 70
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 4
    OnClick = btnBrowseInputFileClick
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 191
    Width = 710
    Height = 175
    Caption = 'Signing options  '
    TabOrder = 5
    object Label4: TLabel
      Left = 10
      Top = 34
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object Label8: TLabel
      Left = 217
      Top = 34
      Width = 75
      Height = 13
      Caption = 'Signature form:'
    end
    object Label2: TLabel
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
    object Label3: TLabel
      Left = 10
      Top = 116
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object cbLevel: TComboBox
      Left = 49
      Top = 30
      Width = 148
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'BES'
      Items.Strings = (
        'BES'
        'EPES'
        'T'
        'C'
        'XType1'
        'XType2'
        'XLType1'
        'XLType2'
        'BaselineB'
        'BaselineT'
        'BaselineLT'
        'BaselineLTA'
        'ExtendedBES'
        'ExtendedEPES'
        'ExtendedT'
        'ExtendedC'
        'ExtendedXType1'
        'ExtendedXType2'
        'ExtendedXLType1'
        'ExtendedXLType2 '
        'A'
        'ExtendedA')
    end
    object cbSignatureType: TComboBox
      Left = 298
      Top = 30
      Width = 87
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'CAdES'
      Items.Strings = (
        'CAdES'
        'XAdES')
    end
    object edSigningCertificate: TEdit
      Left = 10
      Top = 83
      Width = 236
      Height = 21
      TabOrder = 2
    end
    object edCertPassword: TEdit
      Left = 123
      Top = 112
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object GroupBox5: TGroupBox
      Left = 365
      Top = 64
      Width = 335
      Height = 105
      Caption = 'Signing chain  '
      TabOrder = 4
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
    object cbExtended: TCheckBox
      Left = 410
      Top = 32
      Width = 100
      Height = 17
      Caption = 'Extended'
      TabOrder = 5
    end
  end
  object lbSourceFiles: TListBox
    Left = 8
    Top = 57
    Width = 336
    Height = 80
    ItemHeight = 13
    TabOrder = 6
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 370
    Width = 711
    Height = 135
    Caption = 'Additional options  '
    TabOrder = 7
    object GroupBox7: TGroupBox
      Left = 10
      Top = 17
      Width = 335
      Height = 110
      Caption = 'Policy  '
      TabOrder = 0
      object Label5: TLabel
        Left = 10
        Top = 54
        Width = 75
        Height = 13
        Caption = 'Hash algorithm:'
      end
      object Label6: TLabel
        Left = 10
        Top = 27
        Width = 48
        Height = 13
        Caption = 'Identifier:'
      end
      object Label7: TLabel
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
    object cbRequestTimestamp: TCheckBox
      Left = 365
      Top = 83
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 1
    end
    object editTSPServer: TEdit
      Left = 384
      Top = 106
      Width = 296
      Height = 21
      TabOrder = 2
      Text = 'http://'
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


