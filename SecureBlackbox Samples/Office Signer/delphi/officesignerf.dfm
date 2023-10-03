object FormOfficesigner: TFormOfficesigner
  Left = 0
  Top = 0
  Caption = 'Office Signer demo'
  ClientHeight = 486
  ClientWidth = 724
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
    724
    486)
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
      Width = 77
      Height = 13
      Caption = 'Signature Type:'
    end
    object Label2: TLabel
      Left = 275
      Top = 29
      Width = 76
      Height = 13
      Caption = 'Hash Algorithm:'
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
    object cbSignatureType: TComboBox
      Left = 93
      Top = 25
      Width = 135
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
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
      Left = 356
      Top = 25
      Width = 135
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 4
      Text = 'SHA1'
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
  end
  object btnSign: TButton
    Left = 646
    Top = 455
    Width = 70
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 302
    Width = 710
    Height = 140
    Caption = 'Additional options  '
    TabOrder = 4
    object GroupBox3: TGroupBox
      Left = 365
      Top = 22
      Width = 335
      Height = 110
      Caption = 'Signature Info  '
      TabOrder = 0
      object Label7: TLabel
        Left = 10
        Top = 54
        Width = 75
        Height = 13
        Caption = 'Signature Text:'
      end
      object Label11: TLabel
        Left = 10
        Top = 84
        Width = 103
        Height = 13
        Caption = 'Signature Comments:'
      end
      object edSigText: TEdit
        Left = 95
        Top = 50
        Width = 235
        Height = 21
        TabOrder = 0
      end
      object edSigComments: TEdit
        Left = 119
        Top = 80
        Width = 211
        Height = 21
        TabOrder = 1
      end
      object cbIncludeSigInfo: TCheckBox
        Left = 10
        Top = 25
        Width = 229
        Height = 17
        Caption = '  Include'
        TabOrder = 2
      end
    end
    object cbSignDocument: TCheckBox
      Left = 10
      Top = 30
      Width = 293
      Height = 17
      Caption = 'Sign Document'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbSignSignatureOrigin: TCheckBox
      Left = 10
      Top = 57
      Width = 261
      Height = 17
      Caption = 'Sign Signature Origin'
      TabOrder = 2
    end
    object cbSignCoreProperties: TCheckBox
      Left = 10
      Top = 84
      Width = 171
      Height = 17
      Caption = 'Sign Core Properties'
      TabOrder = 3
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


