object FormPdfsignerpkiproxy: TFormPdfsignerpkiproxy
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PDF Signer demo (PKI Proxy)'
  ClientHeight = 456
  ClientWidth = 651
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
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 636
    Height = 13
    Caption = 
      'This sample illustrates the use of PDFSigner component for signi' +
      'ng PDF documents. Please pick the signing certificate and click ' +
      #39'Sign'#39'. '
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
    Width = 636
    Height = 290
    Caption = 'Signing options  '
    TabOrder = 2
    object Label3: TLabel
      Left = 10
      Top = 74
      Width = 182
      Height = 13
      Caption = 'PIN to the PKI Proxy PKCS#11 driver:'
    end
    object btnLoadCerts: TSpeedButton
      Left = 471
      Top = 68
      Width = 160
      Height = 25
      Caption = 'Load certificates...'
      OnClick = btnLoadCertsClick
    end
    object Label4: TLabel
      Left = 10
      Top = 29
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object edStoragePIN: TEdit
      Left = 198
      Top = 71
      Width = 267
      Height = 21
      TabOrder = 2
    end
    object cbLevel: TComboBox
      Left = 50
      Top = 25
      Width = 135
      Height = 21
      Style = csDropDownList
      ItemIndex = 5
      TabOrder = 0
      Text = 'BES'
      Items.Strings = (
        'Generic/Legacy'
        'Baseline B'
        'Baseline T'
        'Baseline LT'
        'Baseline LTA'
        'BES'
        'EPES'
        'LTV')
    end
    object cbVisible: TCheckBox
      Left = 225
      Top = 27
      Width = 97
      Height = 17
      Caption = 'Visible signature'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object lvCerts: TListView
      Left = 2
      Top = 104
      Width = 632
      Height = 184
      Align = alBottom
      Columns = <
        item
          Caption = 'Subject'
          Width = 170
        end
        item
          Caption = 'Issuer'
          Width = 170
        end
        item
          Caption = 'Valid from'
          Width = 70
        end
        item
          Caption = 'Valid to'
          Width = 70
        end
        item
          Caption = 'Algorithm'
          Width = 100
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 3
      ViewStyle = vsReport
    end
  end
  object btnSign: TButton
    Left = 569
    Top = 420
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
  end
  object dlgOpen: TOpenDialog
    Filter = 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*'
    Left = 496
    Top = 40
  end
  object dlgSave: TSaveDialog
    Filter = 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*'
    Left = 568
    Top = 40
  end
  object dlgOpenDLL: TOpenDialog
    Filter = 'PKCS #11 Library (*.dll)|*.dll'
    Left = 544
    Top = 88
  end
end


