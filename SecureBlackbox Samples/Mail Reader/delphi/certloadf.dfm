object FormCertLoad: TFormCertLoad
  Left = 514
  Top = 394
  BorderStyle = bsDialog
  Caption = 'Decryption Certificate Needed'
  ClientHeight = 403
  ClientWidth = 670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    670
    403)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    Left = 15
    Top = 12
    Width = 642
    Height = 13
    Caption = 
      'The message you'#39're attempt to load is encrypted. In order to dec' +
      'rypt it, the following certificate is required (including its pr' +
      'ivate key).'
  end
  object lblIssuer: TLabel
    Left = 50
    Top = 36
    Width = 34
    Height = 13
    Caption = 'Issuer:'
  end
  object lblSerialNumber: TLabel
    Left = 15
    Top = 67
    Width = 69
    Height = 13
    Caption = 'Serial number:'
  end
  object lblKeyID: TLabel
    Left = 48
    Top = 90
    Width = 36
    Height = 13
    Caption = 'Key ID:'
  end
  object edtIssuer: TEdit
    Left = 90
    Top = 33
    Width = 567
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object txtSerialNumber: TStaticText
    Left = 90
    Top = 66
    Width = 567
    Height = 17
    AutoSize = False
    TabOrder = 1
  end
  object txtKeyID: TStaticText
    Left = 90
    Top = 90
    Width = 567
    Height = 17
    AutoSize = False
    TabOrder = 2
  end
  object grpCertificate: TGroupBox
    Left = 8
    Top = 126
    Width = 654
    Height = 238
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Certificate '
    TabOrder = 3
    object lblFilename: TLabel
      Left = 20
      Top = 32
      Width = 46
      Height = 13
      Caption = 'Filename:'
    end
    object lblCertIssuer: TLabel
      Left = 56
      Top = 127
      Width = 34
      Height = 13
      Caption = 'Issuer:'
    end
    object lblCertSerialNumber: TLabel
      Left = 21
      Top = 157
      Width = 69
      Height = 13
      Caption = 'Serial number:'
    end
    object lblCertKeyID: TLabel
      Left = 54
      Top = 180
      Width = 36
      Height = 13
      Caption = 'Key ID:'
    end
    object lblCertPrivateKey: TLabel
      Left = 32
      Top = 204
      Width = 58
      Height = 13
      Caption = 'Private key:'
    end
    object lblPassword: TLabel
      Left = 16
      Top = 59
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object edtFilename: TEdit
      Left = 72
      Top = 29
      Width = 536
      Height = 21
      TabOrder = 0
    end
    object btnSelect: TButton
      Left = 611
      Top = 27
      Width = 25
      Height = 25
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectClick
    end
    object edtCertIssuer: TEdit
      Left = 96
      Top = 124
      Width = 540
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object txtCertSerialNumber: TStaticText
      Left = 96
      Top = 157
      Width = 541
      Height = 17
      AutoSize = False
      TabOrder = 5
    end
    object txtCertKeyID: TStaticText
      Left = 97
      Top = 180
      Width = 540
      Height = 17
      AutoSize = False
      TabOrder = 6
    end
    object txtCertPrivateKey: TStaticText
      Left = 96
      Top = 203
      Width = 541
      Height = 17
      AutoSize = False
      TabOrder = 7
    end
    object btnLoad: TButton
      Left = 290
      Top = 83
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 3
      OnClick = btnLoadClick
    end
    object edtPassword: TEdit
      Left = 72
      Top = 56
      Width = 564
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
  end
  object btnOk: TButton
    Left = 506
    Top = 370
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 587
    Top = 370
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object dlgLoadCert: TOpenDialog
    Filter = 'PKCS#12 Files (*.pfx)|*.pfx|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Load a Certificate'
    Left = 473
    Top = 135
  end
end
