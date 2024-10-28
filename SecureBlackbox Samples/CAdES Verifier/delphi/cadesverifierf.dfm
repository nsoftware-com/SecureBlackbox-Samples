object FormCadesverifier: TFormCadesverifier
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CAdES Verifier demo'
  ClientHeight = 366
  ClientWidth = 698
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
  object lOutputFile: TLabel
    Left = 8
    Top = 111
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbBrowseOutputFile: TSpeedButton
    Left = 366
    Top = 105
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseOutputFileClick
  end
  object Label10: TLabel
    Left = 5
    Top = 15
    Width = 682
    Height = 13
    Caption = 
      'This sample shows processing of CAdES signatures. Please select ' +
      'a .p7s or .p7m file, tune up validation settings, and click '#39'Ver' +
      'ify'#39' when ready.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cbDetached: TCheckBox
    Left = 8
    Top = 84
    Width = 209
    Height = 17
    Caption = 'Detached'
    TabOrder = 0
    OnClick = cbDetachedClick
  end
  object edInputFile: TEdit
    Left = 83
    Top = 45
    Width = 277
    Height = 21
    TabOrder = 1
  end
  object btnVerify: TButton
    Left = 616
    Top = 333
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 2
    OnClick = btnVerifyClick
  end
  object edOutputFile: TEdit
    Left = 83
    Top = 107
    Width = 277
    Height = 21
    TabOrder = 3
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 150
    Width = 683
    Height = 170
    Caption = 'Validation settings  '
    TabOrder = 4
    object GroupBox2: TGroupBox
      Left = 10
      Top = 60
      Width = 330
      Height = 105
      Caption = 'Known Certificates'
      TabOrder = 0
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
      Top = 60
      Width = 330
      Height = 105
      Caption = 'Trusted Certificates'
      TabOrder = 1
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
    object cbIgnoreChainValidationErrors: TCheckBox
      Left = 10
      Top = 28
      Width = 171
      Height = 17
      Caption = 'Ignore chain validation errors'
      TabOrder = 2
    end
    object cbPerformRevocationCheck: TCheckBox
      Left = 195
      Top = 28
      Width = 153
      Height = 17
      Caption = 'Perform revocation check'
      TabOrder = 3
    end
    object cbForceCompleteChainValidation: TCheckBox
      Left = 360
      Top = 28
      Width = 171
      Height = 17
      Caption = 'Force complete chain validation'
      TabOrder = 4
    end
    object cbOfflineMode: TCheckBox
      Left = 554
      Top = 28
      Width = 87
      Height = 17
      Caption = 'Offline Mode'
      TabOrder = 5
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


