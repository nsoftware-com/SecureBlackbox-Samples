object FormSoapsigner: TFormSoapsigner
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'SOAP Signer demo'
  ClientHeight = 478
  ClientWidth = 696
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
    Left = 8
    Top = 51
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 360
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
    Left = 360
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
    Width = 280
    Height = 21
    TabOrder = 1
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 77
    Width = 280
    Height = 21
    TabOrder = 2
  end
  object gbGeneralEnc: TGroupBox
    Left = 8
    Top = 120
    Width = 293
    Height = 130
    Caption = 'General'
    TabOrder = 3
    DesignSize = (
      293
      130)
    object lHashAlgorithm: TLabel
      Left = 16
      Top = 78
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lSignatureType: TLabel
      Left = 16
      Top = 28
      Width = 77
      Height = 13
      Caption = 'Signature Type:'
    end
    object cmbHashAlgorithm: TComboBox
      Left = 107
      Top = 75
      Width = 176
      Height = 22
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Items.Strings = (
        'SHA1'
        'MD5'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160')
    end
    object cmbSignatureType: TComboBox
      Left = 107
      Top = 25
      Width = 176
      Height = 22
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = 'WSS Signature'
      OnChange = cmbSignatureTypeChange
      Items.Strings = (
        'WSS Signature'
        'SOAP Signature ')
    end
    object cbSignBody: TCheckBox
      Left = 107
      Top = 53
      Width = 97
      Height = 17
      Caption = 'Sign Body'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object btnReferences: TButton
    Left = 8
    Top = 441
    Width = 129
    Height = 25
    Caption = 'Additional References'
    TabOrder = 4
    OnClick = btnReferencesClick
  end
  object btnSign: TButton
    Left = 603
    Top = 441
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 0
    OnClick = btnSignClick
  end
  object gbKeyInfo: TGroupBox
    Left = 8
    Top = 256
    Width = 680
    Height = 179
    Caption = 'Key Info'
    TabOrder = 5
    DesignSize = (
      680
      179)
    object Label5: TLabel
      Left = 15
      Top = 148
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object Label3: TLabel
      Left = 15
      Top = 96
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 257
      Top = 113
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object lCertEmbed: TLabel
      Left = 16
      Top = 22
      Width = 122
      Height = 13
      Caption = 'Embed Certificate option:'
    end
    object lKeyName: TLabel
      Left = 16
      Top = 72
      Width = 52
      Height = 13
      Caption = 'Key Name:'
    end
    object gbSigningCertificates: TGroupBox
      Left = 358
      Top = 19
      Width = 312
      Height = 150
      Caption = 'Signing Chain'
      TabOrder = 5
      object lvSigningCertificates: TListView
        Left = 10
        Top = 23
        Width = 214
        Height = 114
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
        Left = 230
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveClick
      end
      object btnAdd: TButton
        Left = 230
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddClick
      end
    end
    object edCertPassword: TEdit
      Left = 128
      Top = 144
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object edSigningCertificate: TEdit
      Left = 15
      Top = 115
      Width = 236
      Height = 21
      TabOrder = 2
    end
    object cbIncludeKey: TCheckBox
      Left = 16
      Top = 31
      Width = 209
      Height = 17
      Caption = 'Include Key (public part)'
      Checked = True
      State = cbChecked
      TabOrder = 0
      Visible = False
    end
    object cmbCertEmbed: TComboBox
      Left = 16
      Top = 38
      Width = 243
      Height = 22
      ItemIndex = 1
      TabOrder = 4
      Text = 'In Binary Security Token'
      Items.Strings = (
        'In Signature'
        'In Binary Security Token'
        'In Signed Binary Security Token,'
        'In Binary Security Token And Signature'
        'None')
    end
    object edKeyName: TEdit
      Left = 80
      Top = 68
      Width = 230
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 223
    end
  end
  object dlgOpenSOAP: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 464
    Top = 120
  end
  object dlgSaveSOAP: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 392
    Top = 120
  end
  object DlgOpen: TOpenDialog
    Left = 520
    Top = 88
  end
end


