object FormAsicsigner: TFormAsicsigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ASiC Signer demo'
  ClientHeight = 381
  ClientWidth = 734
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
  DesignSize = (
    734
    381)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 150
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 388
    Top = 145
    Width = 70
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object lbInputFileName: TLabel
    Left = 8
    Top = 40
    Width = 133
    Height = 13
    Caption = 'Input files list (files to sign):'
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 710
    Height = 13
    Caption = 
      'This sample shows how to create ASiC signatures. Please select i' +
      'nput files, a signing certificate, and the desired ASiC type and' +
      ' level, and click '#39'Sign'#39'.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnSign: TButton
    Left = 653
    Top = 347
    Width = 70
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Sign'
    TabOrder = 5
    OnClick = btnSignClick
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 147
    Width = 313
    Height = 21
    TabOrder = 3
  end
  object bbClear: TButton
    Left = 388
    Top = 57
    Width = 70
    Height = 25
    Caption = 'Clear'
    TabOrder = 1
    TabStop = False
    OnClick = bbClearClick
  end
  object btnBrowseInputFile: TButton
    Left = 388
    Top = 88
    Width = 70
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 2
    TabStop = False
    OnClick = btnBrowseInputFileClick
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 190
    Width = 715
    Height = 147
    Caption = 'Signing options  '
    TabOrder = 4
    object Label4: TLabel
      Left = 525
      Top = 23
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object Label8: TLabel
      Left = 479
      Top = 50
      Width = 75
      Height = 13
      Caption = 'Signature form:'
    end
    object Label2: TLabel
      Left = 10
      Top = 23
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 380
      Top = 17
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label3: TLabel
      Left = 49
      Top = 50
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object lHashAlgorithm: TLabel
      Left = 479
      Top = 77
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object cbLevel: TComboBox
      Left = 560
      Top = 19
      Width = 148
      Height = 21
      Style = csDropDownList
      ItemIndex = 4
      TabOrder = 4
      Text = 'BES'
      Items.Strings = (
        'BaselineB'
        'BaselineT'
        'BaselineLT'
        'BaselineLTA'
        'BES'
        'EPES'
        'T'
        'C'
        'X'
        'XType1'
        'XType2'
        'XL'
        'XLType1'
        'XLType2'
        'A'
        'ExtendedBES'
        'ExtendedEPES'
        'ExtendedT'
        'ExtendedC'
        'ExtendedX'
        'ExtendedXType1'
        'ExtendedXType2'
        'ExtendedXLong'
        'ExtendedXL'
        'ExtendedXLType1'
        'ExtendedXLType2 '
        'ExtendedA')
    end
    object cbSignatureType: TComboBox
      Left = 560
      Top = 46
      Width = 148
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'CAdES'
      Items.Strings = (
        'CAdES'
        'XAdES')
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 19
      Width = 269
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 105
      Top = 46
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object cbExtended: TCheckBox
      Left = 560
      Top = 100
      Width = 90
      Height = 17
      Caption = 'Extended'
      TabOrder = 7
    end
    object cbHashAlgorithm: TComboBox
      Left = 560
      Top = 73
      Width = 148
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 6
      Text = 'SHA256'
      Items.Strings = (
        'SHA1'
        'SHA256'
        'SHA512')
    end
    object cbRequestTimestamp: TCheckBox
      Left = 10
      Top = 91
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 2
    end
    object editTSPServer: TEdit
      Left = 10
      Top = 114
      Width = 364
      Height = 21
      TabOrder = 3
      Text = 'http://'
    end
  end
  object lbSourceFiles: TListBox
    Left = 8
    Top = 57
    Width = 374
    Height = 80
    ItemHeight = 13
    TabOrder = 0
  end
  object dlgOpen: TOpenDialog
    Left = 496
    Top = 40
  end
  object dlgSave: TSaveDialog
    Left = 568
    Top = 40
  end
end


