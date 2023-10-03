object FormJadesverifier: TFormJadesverifier
  Left = 0
  Top = 0
  Caption = 'JAdES Verifier demo'
  ClientHeight = 394
  ClientWidth = 699
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
    Width = 438
    Height = 13
    Caption = 
      'This sample illustrates the use of JAdESVerifier component for v' +
      'alidating JAdES signatures. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 421
    Top = 102
    Width = 241
    Height = 39
    Caption = 
      'For detached signature specify the payload here. Otherwise, the ' +
      'payload would be extracted from the signature into this field.'
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
    Top = 80
    Width = 94
    Height = 13
    Caption = 'Payload (as string):'
  end
  object edInputFile: TEdit
    Left = 83
    Top = 45
    Width = 277
    Height = 21
    TabOrder = 0
  end
  object btnVerify: TButton
    Left = 608
    Top = 361
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 1
    OnClick = btnVerifyClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 170
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
  object mmPayload: TMemo
    Left = 8
    Top = 99
    Width = 389
    Height = 54
    Lines.Strings = (
      'Hello, world!')
    TabOrder = 3
  end
  object dlgOpen: TOpenDialog
    Left = 473
    Top = 32
  end
end


