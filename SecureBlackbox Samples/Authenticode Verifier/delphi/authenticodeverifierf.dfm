object FormAuthenticodeverifier: TFormAuthenticodeverifier
  Left = 366
  Top = 238
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Authenticode Verifier'
  ClientHeight = 354
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 15
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
    Width = 52
    Height = 15
    Caption = 'Input File:'
  end
  object edtInputFile: TEdit
    Left = 72
    Top = 39
    Width = 418
    Height = 23
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
          Caption = 'Signer'
          Width = 250
        end
        item
          Alignment = taCenter
          Caption = 'Algorithm'
          Width = 80
        end
        item
          Alignment = taCenter
          Caption = 'Validity'
          Width = 100
        end
        item
          Alignment = taCenter
          Caption = 'Timestamp'
          Width = 80
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnDeletion = lvwSignaturesDeletion
      OnSelectItem = lvwSignaturesSelectItem
    end
    object grpSignature: TGroupBox
      Left = 12
      Top = 109
      Width = 256
      Height = 126
      Caption = ' Signature Info '
      TabOrder = 1
      object lblDescription: TLabel
        Left = 9
        Top = 24
        Width = 63
        Height = 15
        Caption = 'Description:'
      end
      object lblUrl: TLabel
        Left = 43
        Top = 52
        Width = 24
        Height = 15
        Caption = 'URL:'
      end
      object lblStatement: TLabel
        Left = 12
        Top = 75
        Width = 57
        Height = 15
        Caption = 'Statement:'
      end
      object lblClaimedTime: TLabel
        Left = 40
        Top = 99
        Width = 29
        Height = 15
        Caption = 'Time:'
      end
      object edtDescription: TEdit
        Left = 72
        Top = 21
        Width = 178
        Height = 23
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object txtUrl: TStaticText
        Left = 72
        Top = 52
        Width = 178
        Height = 17
        AutoSize = False
        TabOrder = 1
      end
      object txtStatement: TStaticText
        Left = 72
        Top = 75
        Width = 178
        Height = 17
        AutoSize = False
        TabOrder = 2
      end
      object txtClaimedTime: TStaticText
        Left = 72
        Top = 99
        Width = 178
        Height = 17
        AutoSize = False
        TabOrder = 3
      end
    end
    object grpTimestampInfo: TGroupBox
      Left = 274
      Top = 109
      Width = 270
      Height = 126
      Caption = ' Timestamp Info '
      TabOrder = 2
      object lblTimestamper: TLabel
        Left = 27
        Top = 24
        Width = 36
        Height = 15
        Caption = 'Signer:'
      end
      object lblAlgorithm: TLabel
        Left = 12
        Top = 52
        Width = 57
        Height = 15
        Caption = 'Algorithm:'
      end
      object lblType: TLabel
        Left = 33
        Top = 75
        Width = 27
        Height = 15
        Caption = 'Type:'
      end
      object lblValidatedTime: TLabel
        Left = 35
        Top = 99
        Width = 29
        Height = 15
        Caption = 'Time:'
      end
      object edtTimestamper: TEdit
        Left = 67
        Top = 21
        Width = 192
        Height = 23
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object txtAlgorithm: TStaticText
        Left = 67
        Top = 52
        Width = 192
        Height = 17
        AutoSize = False
        TabOrder = 1
      end
      object txtType: TStaticText
        Left = 67
        Top = 75
        Width = 192
        Height = 17
        AutoSize = False
        TabOrder = 2
      end
      object txtValidatedTime: TStaticText
        Left = 67
        Top = 99
        Width = 192
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
    Left = 464
    Top = 65
  end
end


