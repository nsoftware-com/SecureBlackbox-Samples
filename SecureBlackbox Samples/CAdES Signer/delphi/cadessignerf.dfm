object FormCadessigner: TFormCadessigner
  Left = 0
  Top = 0
  Caption = 'CAdES Signer demo'
  ClientHeight = 437
  ClientWidth = 691
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
  object lbInputFile: TLabel
    Left = 8
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 350
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object Label1: TLabel
    Left = 8
    Top = 84
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 350
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 589
    Height = 13
    Caption = 
      'This sample shows how to create CAdES signatures. Please select ' +
      'an input file, tune up the signing options, and click '#39'Sign'#39'.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputFile: TEdit
    Left = 77
    Top = 45
    Width = 260
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 77
    Top = 80
    Width = 260
    Height = 21
    TabOrder = 1
  end
  object btnSign: TButton
    Left = 608
    Top = 404
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 2
    OnClick = btnSignClick
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 127
    Width = 675
    Height = 180
    Caption = 'Signing options  '
    TabOrder = 3
    object Label4: TLabel
      Left = 10
      Top = 34
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object Label2: TLabel
      Left = 10
      Top = 72
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 252
      Top = 89
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label3: TLabel
      Left = 10
      Top = 124
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object lbHashAlgorithm: TLabel
      Left = 227
      Top = 33
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object edSigningCertificate: TEdit
      Left = 10
      Top = 91
      Width = 236
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 123
      Top = 120
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object GroupBox5: TGroupBox
      Left = 335
      Top = 72
      Width = 335
      Height = 105
      Caption = 'Signing Certificates'
      TabOrder = 2
      object lvSigningCertificates: TListView
        Left = 5
        Top = 24
        Width = 250
        Height = 75
        Columns = <
          item
            Caption = 'Serial'
            Width = 100
          end
          item
            Caption = 'Issuer'
            Width = 145
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemoveCert: TButton
        Left = 260
        Top = 55
        Width = 70
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveCertClick
      end
      object btnAddCert: TButton
        Left = 260
        Top = 24
        Width = 70
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddCertClick
      end
    end
    object cbLevel: TComboBox
      Left = 45
      Top = 30
      Width = 148
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'BES'
      Items.Strings = (
        'BES'
        'EPES'
        'T'
        'C'
        'XType1'
        'XType2'
        'XLType1'
        'XLType2'
        'BaselineB'
        'BaselineT'
        'BaselineLT'
        'BaselineLTA'
        'ExtendedBES'
        'ExtendedEPES'
        'ExtendedT'
        'ExtendedC'
        'ExtendedXType1'
        'ExtendedXType2'
        'ExtendedXLType1'
        'ExtendedXLType2 '
        'ExtendedA')
    end
    object cmbHashAlgorithm: TComboBox
      Left = 308
      Top = 28
      Width = 149
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      Items.Strings = (
        'SHA1'
        'MD5'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160')
    end
    object cbDetached: TCheckBox
      Left = 507
      Top = 30
      Width = 121
      Height = 17
      Caption = 'Detached'
      TabOrder = 5
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 313
    Width = 675
    Height = 75
    Caption = 'Additional options  '
    TabOrder = 4
    object cbRequestTimestamp: TCheckBox
      Left = 17
      Top = 23
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 0
    end
    object editTSPServer: TEdit
      Left = 26
      Top = 46
      Width = 296
      Height = 21
      TabOrder = 1
      Text = 'http://'
    end
  end
  object dlgOpenFile: TOpenDialog
    Left = 568
    Top = 48
  end
  object dlgSaveFile: TSaveDialog
    Left = 480
    Top = 48
  end
  object DlgOpenCert: TOpenDialog
    Filter = 
      'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*' +
      '.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX'
    Title = 'Select certificate file'
    Left = 648
    Top = 48
  end
end


