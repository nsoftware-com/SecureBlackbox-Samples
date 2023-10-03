object FormMessagesigner: TFormMessagesigner
  Left = 0
  Top = 0
  Caption = 'Message Signer demo'
  ClientHeight = 368
  ClientWidth = 728
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
    Width = 488
    Height = 13
    Caption = 
      'This sample illustrates the use of MessageSigner component to cr' +
      'eate PKCS#7-compliant signed files. '
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
  object btnSign: TButton
    Left = 643
    Top = 332
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 0
    OnClick = btnSignClick
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 119
    Width = 710
    Height = 194
    Caption = 'Signing options  '
    TabOrder = 1
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
    object lbSignatureType: TLabel
      Left = 10
      Top = 34
      Width = 75
      Height = 13
      Caption = 'Signature type:'
    end
    object lbHashAlgorithm: TLabel
      Left = 325
      Top = 34
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
      Left = 365
      Top = 72
      Width = 335
      Height = 105
      Caption = 'Signing Chain'
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
    object cmbSignatureType: TComboBox
      Left = 100
      Top = 30
      Width = 189
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'PKCS1 Detached'
        'PKCS7 Detached'
        'PKCS7 Enveloping')
    end
    object cmbHashAlgorithm: TComboBox
      Left = 410
      Top = 30
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
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 245
    Height = 21
    TabOrder = 2
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 80
    Width = 245
    Height = 21
    TabOrder = 3
  end
  object dlgOpenFile: TOpenDialog
    Left = 232
    Top = 8
  end
  object dlgSaveFile: TSaveDialog
    Left = 152
    Top = 8
  end
  object DlgOpenCert: TOpenDialog
    Filter = 
      'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*' +
      '.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX'
    Title = 'Select certificate file'
    Left = 568
    Top = 232
  end
end


