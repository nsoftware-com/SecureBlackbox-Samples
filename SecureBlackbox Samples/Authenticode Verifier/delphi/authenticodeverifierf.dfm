object FormAuthenticodeverifier: TFormAuthenticodeverifier
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Authenticode Verifier'
  ClientHeight = 354
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 14
    Top = 14
    Width = 557
    Height = 13
    Caption = 
      'This sample showcases AuthenticodeVerifier'#39's facilities in valid' +
      'ating Authenticode-compliant signed EXE and DLL files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblInputFile: TLabel
    Left = 14
    Top = 42
    Width = 49
    Height = 13
    Caption = 'Input File:'
  end
  object edtInputFile: TEdit
    Left = 72
    Top = 39
    Width = 418
    Height = 21
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 496
    Top = 37
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnVerify: TButton
    Left = 211
    Top = 66
    Width = 150
    Height = 25
    Caption = 'Verify'
    TabOrder = 2
    OnClick = btnVerifyClick
  end
  object grpSignatures: TGroupBox
    Left = 14
    Top = 97
    Width = 557
    Height = 245
    Caption = ' Signatures '
    TabOrder = 3
    object lvwSignatures: TListView
      Left = 12
      Top = 24
      Width = 532
      Height = 79
      Columns = <
        item
          Caption = '#'
        end
        item
          Alignment = taCenter
          Caption = 'Algorithm'
          Width = 80
        end
        item
          Alignment = taCenter
          Caption = 'Validity'
          Width = 150
        end
        item
          Alignment = taCenter
          Caption = 'Validated Signing Time'
          Width = 150
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvwSignaturesSelectItem
    end
    object grpSignature: TGroupBox
      Left = 12
      Top = 109
      Width = 532
      Height = 126
      Caption = ' Signature Info '
      TabOrder = 1
      object lblDescription: TLabel
        Left = 9
        Top = 24
        Width = 57
        Height = 13
        Caption = 'Description:'
      end
      object lblUrl: TLabel
        Left = 43
        Top = 52
        Width = 23
        Height = 13
        Caption = 'URL:'
      end
      object lblStatement: TLabel
        Left = 12
        Top = 75
        Width = 54
        Height = 13
        Caption = 'Statement:'
      end
      object lblClaimedTime: TLabel
        Left = 40
        Top = 99
        Width = 26
        Height = 13
        Caption = 'Time:'
      end
      object edtDescription: TEdit
        Left = 72
        Top = 21
        Width = 281
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object txtUrl: TStaticText
        Left = 72
        Top = 52
        Width = 361
        Height = 17
        AutoSize = False
        TabOrder = 1
      end
      object txtStatement: TStaticText
        Left = 72
        Top = 75
        Width = 361
        Height = 17
        AutoSize = False
        TabOrder = 2
      end
      object txtClaimedTime: TStaticText
        Left = 72
        Top = 99
        Width = 361
        Height = 17
        AutoSize = False
        TabOrder = 3
      end
    end
  end
  object dlgOpenExe: TOpenDialog
    Filter = 'Executables (*.exe, *.dll)|*.exe;*.dll|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select Input Executable'
    Left = 496
    Top = 65
  end
  object Verifier: TsbxAuthenticodeVerifier
    OnSignatureFound = VerifierSignatureFound
    OnTimestampFound = VerifierTimestampFound
    Left = 416
    Top = 64
  end
end


