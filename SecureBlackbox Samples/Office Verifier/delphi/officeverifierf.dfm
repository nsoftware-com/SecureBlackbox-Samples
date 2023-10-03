object FormOfficeverifier: TFormOfficeverifier
  Left = 0
  Top = 0
  Caption = 'Office Verifier demo'
  ClientHeight = 334
  ClientWidth = 702
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
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 366
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object Label10: TLabel
    Left = 5
    Top = 15
    Width = 473
    Height = 13
    Caption = 
      'This demo gives an example of using OfficeVerifier component to ' +
      'process signed office documents.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputFile: TEdit
    Left = 83
    Top = 45
    Width = 277
    Height = 21
    TabOrder = 0
  end
  object btnVerify: TButton
    Left = 616
    Top = 301
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 1
    OnClick = btnVerifyClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 94
    Width = 683
    Height = 185
    Caption = 'Validation settings  '
    TabOrder = 2
    object cbPerformRevocationCheck: TCheckBox
      Left = 200
      Top = 28
      Width = 153
      Height = 17
      Caption = 'Perform revocation check'
      TabOrder = 0
    end
    object cbIgnoreChainValidationErrors: TCheckBox
      Left = 10
      Top = 28
      Width = 171
      Height = 17
      Caption = 'Ignore chain validation errors'
      TabOrder = 1
    end
    object cbForceCompleteChainValidation: TCheckBox
      Left = 370
      Top = 28
      Width = 171
      Height = 17
      Caption = 'Force complete chain validation'
      TabOrder = 2
    end
    object GroupBox2: TGroupBox
      Left = 10
      Top = 69
      Width = 330
      Height = 105
      Caption = 'Known Certificates'
      TabOrder = 3
      object lvKnownCertificates: TListView
        Left = 5
        Top = 24
        Width = 240
        Height = 75
        Columns = <
          item
            Caption = 'Serial'
            Width = 100
          end
          item
            Caption = 'Issuer'
            Width = 130
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemoveKnown: TButton
        Left = 250
        Top = 55
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveKnownClick
      end
      object btnAddKnown: TButton
        Left = 250
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddKnownClick
      end
    end
    object GroupBox4: TGroupBox
      Left = 345
      Top = 69
      Width = 330
      Height = 105
      Caption = 'Trusted Certificates'
      TabOrder = 4
      object lvTrustedCertificates: TListView
        Left = 5
        Top = 24
        Width = 240
        Height = 75
        Columns = <
          item
            Caption = 'Serial'
            Width = 100
          end
          item
            Caption = 'Issuer'
            Width = 130
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemoveTrusted: TButton
        Left = 250
        Top = 55
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveTrustedClick
      end
      object btnAddTrusted: TButton
        Left = 250
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddTrustedClick
      end
    end
  end
  object dlgOpen: TOpenDialog
    Left = 569
    Top = 32
  end
  object dlgSave: TSaveDialog
    Left = 632
    Top = 32
  end
end


