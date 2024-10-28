object FormPgpreader: TFormPgpreader
  Left = 269
  Top = 201
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PGP Reader demo'
  ClientHeight = 330
  ClientWidth = 509
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
  object lbInputFileName: TLabel
    Left = 8
    Top = 48
    Width = 125
    Height = 13
    Caption = 'File to decrypt and verify:'
  end
  object Label1: TLabel
    Left = 8
    Top = 88
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object Label10: TLabel
    Left = 8
    Top = 10
    Width = 465
    Height = 26
    AutoSize = False
    Caption = 
      'This sample is a younger PGP Desktop brother that illustrates th' +
      'e use of PGPReader component for processing protected OpenPGP fi' +
      'les. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object editInputFile: TEdit
    Left = 139
    Top = 45
    Width = 280
    Height = 21
    TabOrder = 0
  end
  object btnBrowseInputFile: TButton
    Left = 423
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 1
    OnClick = btnBrowseInputFileClick
  end
  object btnDecrypt: TButton
    Left = 388
    Top = 297
    Width = 110
    Height = 25
    Caption = 'Decrypt and verify'
    TabOrder = 4
    OnClick = btnDecryptClick
  end
  object editOutputFile: TEdit
    Left = 139
    Top = 85
    Width = 280
    Height = 21
    TabOrder = 2
  end
  object btnBrowseOutFile: TButton
    Left = 423
    Top = 83
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 3
    OnClick = btnBrowseOutputFileClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 125
    Width = 490
    Height = 156
    Caption = 'Keys  '
    TabOrder = 5
    object lbDecryptKeyList: TLabel
      Left = 10
      Top = 69
      Width = 109
      Height = 13
      Caption = 'Key list for decryption:'
      Enabled = False
    end
    object lbVerifyKeyList: TLabel
      Left = 10
      Top = 104
      Width = 84
      Height = 13
      Caption = 'Key for verifying:'
      Enabled = False
    end
    object btnBrowseKeyringFile: TButton
      Left = 343
      Top = 22
      Width = 138
      Height = 25
      Caption = 'Browse for keyring files ...'
      TabOrder = 0
      OnClick = btnBrowseKeyringFileClick
    end
    object cbDecryptKeySelect: TComboBox
      Left = 131
      Top = 65
      Width = 350
      Height = 21
      Style = csDropDownList
      Enabled = False
      TabOrder = 1
    end
    object cbVerifyKeySelect: TComboBox
      Left = 131
      Top = 100
      Width = 350
      Height = 21
      Style = csDropDownList
      Enabled = False
      TabOrder = 2
    end
    object cbAutoKeySelect: TCheckBox
      Left = 10
      Top = 25
      Width = 250
      Height = 17
      Caption = 'Automatically select appropriate keys'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbAutoKeySelectClick
    end
  end
  object dlgOpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 200
    Top = 80
  end
  object dlgSaveDialog: TSaveDialog
    Left = 232
    Top = 80
  end
end


