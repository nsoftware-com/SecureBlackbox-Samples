object FormSimplepdfsigner: TFormSimplepdfsigner
  Left = 0
  Top = 0
  Caption = 'Simple PDF Signer demo'
  ClientHeight = 531
  ClientWidth = 729
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
    Width = 710
    Height = 230
    Caption = 'Signing options  '
    TabOrder = 2
    object Label3: TLabel
      Left = 15
      Top = 25
      Width = 85
      Height = 13
      Caption = 'Signing certificate'
    end
    object sbCertFile: TSpeedButton
      Left = 297
      Top = 73
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbCertFileClick
    end
    object Label5: TLabel
      Left = 390
      Top = 79
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object sbPKCS11File: TSpeedButton
      Left = 297
      Top = 133
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbPKCS11FileClick
    end
    object Label2: TLabel
      Left = 390
      Top = 139
      Width = 21
      Height = 13
      Caption = 'PIN:'
    end
    object edCertFile: TEdit
      Left = 55
      Top = 75
      Width = 236
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 500
      Top = 75
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object rbCertFile: TRadioButton
      Left = 35
      Top = 50
      Width = 89
      Height = 17
      Caption = 'Certificate file:'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object rbPKCS11: TRadioButton
      Left = 35
      Top = 110
      Width = 113
      Height = 17
      Caption = 'PKCS11 storage file:'
      TabOrder = 3
    end
    object rbWin32: TRadioButton
      Left = 35
      Top = 170
      Width = 113
      Height = 17
      Caption = 'Win32 storage:'
      TabOrder = 4
    end
    object edPKCS11File: TEdit
      Left = 55
      Top = 135
      Width = 236
      Height = 21
      TabOrder = 5
    end
    object edPKCS11PIN: TEdit
      Left = 417
      Top = 135
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 6
    end
    object edWin32Store: TEdit
      Left = 55
      Top = 195
      Width = 236
      Height = 21
      TabOrder = 7
      Text = 'My'
    end
  end
  object btnSign: TButton
    Left = 648
    Top = 498
    Width = 70
    Height = 25
    Caption = 'Sign'
    TabOrder = 3
    OnClick = btnSignClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 364
    Width = 710
    Height = 117
    Caption = 'Additional options  '
    TabOrder = 4
    object lAuthorName: TLabel
      Left = 10
      Top = 23
      Width = 73
      Height = 13
      Caption = 'Author'#39's name:'
    end
    object lReason: TLabel
      Left = 10
      Top = 63
      Width = 93
      Height = 13
      Caption = 'Reason for signing:'
    end
    object editAuthorName: TEdit
      Left = 10
      Top = 38
      Width = 249
      Height = 21
      TabOrder = 0
    end
    object cbReason: TComboBox
      Left = 10
      Top = 78
      Width = 290
      Height = 21
      TabOrder = 1
      Text = '<none>'
      Items.Strings = (
        'I am the author of this document'
        
          'I agree to the terms defined by placement of my signature on thi' +
          's document'
        'I have reviewed this document'
        'I attest to the accuracy and integrity of this document'
        'I am approving this document')
    end
    object editTSPServer: TEdit
      Left = 360
      Top = 78
      Width = 296
      Height = 21
      TabOrder = 2
      Text = 'http://'
    end
    object cbRequestTimestamp: TCheckBox
      Left = 349
      Top = 55
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 3
    end
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


