object FormPdfsigner: TFormPdfsigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PDF Signer demo'
  ClientHeight = 332
  ClientWidth = 683
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
    Left = 355
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
    Left = 355
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 279
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 80
    Width = 279
    Height = 21
    TabOrder = 1
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 121
    Width = 665
    Height = 160
    Caption = 'Signing options  '
    TabOrder = 2
    object Label3: TLabel
      Left = 10
      Top = 28
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 347
      Top = 23
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label5: TLabel
      Left = 49
      Top = 56
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label7: TLabel
      Left = 444
      Top = 29
      Width = 77
      Height = 13
      Caption = 'Signature Type:'
    end
    object Label4: TLabel
      Left = 490
      Top = 56
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object Label6: TLabel
      Left = 445
      Top = 83
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 25
      Width = 236
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 105
      Top = 52
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object cbSignatureType: TComboBox
      Left = 530
      Top = 25
      Width = 127
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 7
      Text = 'PAdES'
      OnChange = cbSignatureTypeChange
      Items.Strings = (
        'Legacy'
        'PAdES'
        'DocumentTimestamp')
    end
    object cbLevel: TComboBox
      Left = 530
      Top = 52
      Width = 127
      Height = 21
      Style = csDropDownList
      ItemIndex = 5
      TabOrder = 2
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
      Left = 530
      Top = 106
      Width = 97
      Height = 17
      Caption = 'Visible signature'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object cbHashAlgorithm: TComboBox
      Left = 530
      Top = 79
      Width = 127
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 3
      Text = 'SHA256'
      Items.Strings = (
        'SHA1'
        'MD5'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160')
    end
    object cbRequestTimestamp: TCheckBox
      Left = 10
      Top = 104
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 5
    end
    object editTSPServer: TEdit
      Left = 10
      Top = 129
      Width = 331
      Height = 21
      TabOrder = 6
      Text = 'http://'
    end
  end
  object btnSign: TButton
    Left = 603
    Top = 298
    Width = 70
    Height = 25
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
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


