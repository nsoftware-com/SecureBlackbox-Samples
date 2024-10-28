object FormCreateCert: TFormCreateCert
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Create Certificate'
  ClientHeight = 526
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 5
    Top = 5
    Width = 315
    Height = 476
    Caption = 'Parameters  '
    TabOrder = 0
    object Label1: TLabel
      Left = 61
      Top = 168
      Width = 33
      Height = 13
      Caption = 'Group:'
    end
    object Label2: TLabel
      Left = 15
      Top = 28
      Width = 78
      Height = 13
      Caption = 'Public algorithm:'
    end
    object Label3: TLabel
      Left = 18
      Top = 98
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lblSelectPublicKeyLen: TLabel
      Left = 35
      Top = 133
      Width = 58
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = 'Key length: '
      ParentBiDiMode = False
    end
    object lblCurve: TLabel
      Left = 17
      Top = 63
      Width = 77
      Height = 13
      Caption = 'Curve (for EC) :'
      Enabled = False
    end
    object edGroup: TEdit
      Left = 100
      Top = 165
      Width = 145
      Height = 21
      TabOrder = 0
    end
    object cbPublicAlgorithm: TComboBox
      Left = 100
      Top = 25
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'RSA'
      OnChange = cbPublicAlgorithmChange
      Items.Strings = (
        'RSA'
        'DSA'
        'EC')
    end
    object cbHashAlgorithm: TComboBox
      Left = 100
      Top = 95
      Width = 173
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 2
      Text = 'SHA1'
      Items.Strings = (
        'MD2'
        'MD5'
        'SHA1'
        'SHA224'
        'SHA256'
        'SHA384'
        'SHA512')
    end
    object gbSubject: TGroupBox
      Left = 15
      Top = 200
      Width = 282
      Height = 270
      Caption = 'Subject parameters '
      TabOrder = 3
      object lblCountry: TLabel
        Left = 10
        Top = 25
        Width = 43
        Height = 13
        Caption = 'Country:'
      end
      object lblState: TLabel
        Left = 10
        Top = 65
        Width = 87
        Height = 13
        Caption = 'State or Province:'
      end
      object lblLocality: TLabel
        Left = 10
        Top = 105
        Width = 43
        Height = 13
        Caption = 'Locality: '
      end
      object lblOrganization: TLabel
        Left = 10
        Top = 145
        Width = 65
        Height = 13
        Caption = 'Organization:'
      end
      object lblOrganizationUnit: TLabel
        Left = 10
        Top = 185
        Width = 87
        Height = 13
        Caption = 'Organization Unit:'
      end
      object lblCommonName: TLabel
        Left = 10
        Top = 225
        Width = 75
        Height = 13
        Caption = 'Common Name:'
      end
      object edStateS: TEdit
        Left = 10
        Top = 80
        Width = 260
        Height = 21
        TabOrder = 0
      end
      object edLocalityS: TEdit
        Left = 10
        Top = 120
        Width = 260
        Height = 21
        TabOrder = 1
      end
      object edOrganizationS: TEdit
        Left = 10
        Top = 160
        Width = 260
        Height = 21
        TabOrder = 2
      end
      object edOrganizationUnitS: TEdit
        Left = 10
        Top = 200
        Width = 260
        Height = 21
        TabOrder = 3
      end
      object edCommonNameS: TEdit
        Left = 10
        Top = 240
        Width = 260
        Height = 21
        TabOrder = 4
      end
      object cbCountryS: TComboBox
        Left = 10
        Top = 40
        Width = 260
        Height = 21
        Style = csDropDownList
        DropDownCount = 20
        TabOrder = 5
      end
    end
    object cbCurve: TComboBox
      Left = 100
      Top = 60
      Width = 122
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemIndex = 0
      TabOrder = 4
      Items.Strings = (
        ''
        'SECP112R1'
        'SECT113R1'
        'SECP128R1'
        'SECT131R1'
        'SECP160K1'
        'SECT163K1'
        'C2PNB176W1'
        'C2TNB191V1'
        'SECP192K1'
        'SECT193R1'
        'C2PNB208W1'
        'SECP224K1'
        'SECT233K1'
        'SECT239K1'
        'SECP256K1'
        'C2PNB272W1'
        'SECT283K1'
        'C2PNB304W1'
        'C2TNB359V1'
        'C2PNB368W1'
        'SECP384R1'
        'SECT409K1'
        'C2TNB431R1'
        'BRAINPOOLP512R1'
        'SECP521R1'
        'SECT571K1')
    end
    object edKeyLength: TEdit
      Left = 100
      Top = 130
      Width = 69
      Height = 21
      TabOrder = 5
      Text = '1024'
    end
  end
  object btnOk: TButton
    Left = 156
    Top = 490
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 245
    Top = 490
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
