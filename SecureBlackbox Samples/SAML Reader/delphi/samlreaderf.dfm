object FormSAMLReader: TFormSAMLReader
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SAMLReader demo'
  ClientHeight = 668
  ClientWidth = 600
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
  object gbSecurity: TGroupBox
    Left = 16
    Top = 8
    Width = 569
    Height = 137
    Caption = 'Security settings:'
    TabOrder = 0
    object lblEncCert: TLabel
      Left = 15
      Top = 63
      Width = 107
      Height = 13
      Caption = 'Decryption certificate:'
    end
    object lblSigningCert: TLabel
      Left = 25
      Top = 27
      Width = 97
      Height = 13
      Caption = 'Verifying certificate:'
    end
    object lblCertPassword: TLabel
      Left = 72
      Top = 99
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object editDecryptionCert: TEdit
      Left = 128
      Top = 60
      Width = 337
      Height = 21
      TabOrder = 2
    end
    object btnBrowseEncCert: TButton
      Left = 471
      Top = 58
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 4
      OnClick = btnBrowseEncCertClick
    end
    object editVerifyingCert: TEdit
      Left = 128
      Top = 24
      Width = 337
      Height = 21
      TabOrder = 1
    end
    object btnBrowseSigningCert: TButton
      Left = 471
      Top = 22
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 0
      OnClick = btnBrowseSigningCertClick
    end
    object editPassword: TEdit
      Left = 128
      Top = 96
      Width = 129
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object gbProcess: TGroupBox
    Left = 16
    Top = 151
    Width = 569
    Height = 250
    Caption = 'Process SAML message'
    TabOrder = 1
    object memoInput: TMemo
      Left = 19
      Top = 24
      Width = 527
      Height = 177
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object btnGo: TButton
      Left = 471
      Top = 215
      Width = 75
      Height = 25
      Caption = 'Go!'
      TabOrder = 1
      OnClick = btnGoClick
    end
  end
  object memoOutput: TMemo
    Left = 16
    Top = 416
    Width = 569
    Height = 233
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object dlgVerifyingCert: TOpenDialog
    Filter = 
      'PEM-encoded certificates (*.pem)|*.pem|DER-encoded certificates ' +
      '(*.cer)|*.cer|All Files (*.*)|*.*'
    Left = 504
    Top = 32
  end
  object dlgDecryptionCert: TOpenDialog
    Filter = 
      'PKCS12 certificates (*.pfx)|*.pfx|PEM-encoded certificates (*.pe' +
      'm)|*.pem|DER-encoded certificates (*.cer)|*.cer|All Files (*.*)|' +
      '*.*'
    Left = 512
    Top = 80
  end
end


