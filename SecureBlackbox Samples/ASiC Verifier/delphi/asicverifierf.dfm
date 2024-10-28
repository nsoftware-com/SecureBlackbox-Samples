object FormAsicverifier: TFormAsicverifier
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ASiC Verifier demo'
  ClientHeight = 391
  ClientWidth = 694
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
  object Label1: TLabel
    Left = 8
    Top = 79
    Width = 63
    Height = 13
    Caption = 'Output path:'
  end
  object sbBrowseOutputPath: TSpeedButton
    Left = 366
    Top = 73
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseOutputPathClick
  end
  object Label10: TLabel
    Left = 5
    Top = 15
    Width = 685
    Height = 13
    Caption = 
      'This sample shows how to process signed ASiC containers. Please ' +
      'select an ASiC file, tune up validation settings, and click '#39'Ver' +
      'ify'#39' when ready. '
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
    Left = 613
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 1
    OnClick = btnVerifyClick
  end
  object edOutputPath: TEdit
    Left = 83
    Top = 75
    Width = 277
    Height = 21
    TabOrder = 2
  end
  object GroupBox3: TGroupBox
    Left = 5
    Top = 113
    Width = 683
    Height = 225
    Caption = 'Validation settings  '
    TabOrder = 3
    object Label2: TLabel
      Left = 10
      Top = 32
      Width = 82
      Height = 13
      Caption = 'Extraction Mode:'
    end
    object cbPerformRevocationCheck: TCheckBox
      Left = 195
      Top = 68
      Width = 153
      Height = 17
      Caption = 'Perform revocation check'
      TabOrder = 0
    end
    object cbIgnoreChainValidationErrors: TCheckBox
      Left = 10
      Top = 68
      Width = 171
      Height = 17
      Caption = 'Ignore chain validation errors'
      TabOrder = 1
    end
    object cbForceCompleteChainValidation: TCheckBox
      Left = 360
      Top = 68
      Width = 171
      Height = 17
      Caption = 'Force complete chain validation'
      TabOrder = 2
    end
    object cbExtractionMode: TComboBox
      Left = 98
      Top = 29
      Width = 121
      Height = 21
      ItemIndex = 0
      TabOrder = 3
      Text = 'None'
      Items.Strings = (
        'None'
        'All'
        'Signed'
        'SignedAndValid')
    end
    object GroupBox2: TGroupBox
      Left = 7
      Top = 109
      Width = 330
      Height = 105
      Caption = 'Known Certificates'
      TabOrder = 4
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
      Top = 109
      Width = 330
      Height = 105
      Caption = 'Trusted Certificates'
      TabOrder = 5
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
    object cbOfflineMode: TCheckBox
      Left = 554
      Top = 68
      Width = 87
      Height = 17
      Caption = 'Offline Mode'
      TabOrder = 6
    end
  end
  object DlgOpen: TOpenDialog
    Left = 601
    Top = 32
  end
end


