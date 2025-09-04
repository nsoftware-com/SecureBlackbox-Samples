object FormSAMLWriter: TFormSAMLWriter
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SAMLWriter demo'
  ClientHeight = 681
  ClientWidth = 601
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
  object gbSecurity: TGroupBox
    Left = 16
    Top = 143
    Width = 569
    Height = 249
    Caption = 'Security settings:'
    TabOrder = 0
    object lblEncCert: TLabel
      Left = 32
      Top = 200
      Width = 106
      Height = 13
      Caption = 'Encryption certificate:'
    end
    object lblSigningCert: TLabel
      Left = 49
      Top = 67
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object lblCertPassword: TLabel
      Left = 88
      Top = 101
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object lblHashAlg: TLabel
      Left = 332
      Top = 101
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object editEncCert: TEdit
      Left = 152
      Top = 197
      Width = 313
      Height = 21
      TabOrder = 6
    end
    object btnBrowseEncCert: TButton
      Left = 471
      Top = 195
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 7
      OnClick = btnBrowseEncCertClick
    end
    object editSigningCert: TEdit
      Left = 152
      Top = 64
      Width = 313
      Height = 21
      TabOrder = 1
    end
    object btnBrowseSigningCert: TButton
      Left = 471
      Top = 62
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 2
      OnClick = btnBrowseSigningCertClick
    end
    object editPassword: TEdit
      Left = 152
      Top = 98
      Width = 137
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
    object cbHashAlg: TComboBox
      Left = 416
      Top = 98
      Width = 130
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'SHA256'
      Items.Strings = (
        'SHA256'
        'SHA384'
        'SHA512'
        'SHA1')
    end
    object cbSign: TCheckBox
      Left = 19
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Sign the output'
      TabOrder = 0
    end
    object cbEncrypt: TCheckBox
      Left = 19
      Top = 166
      Width = 270
      Height = 17
      Caption = 'Encrypt the output (assertions only):'
      TabOrder = 5
    end
  end
  object gbGeneral: TGroupBox
    Left = 16
    Top = 8
    Width = 569
    Height = 129
    Caption = 'General settings:'
    TabOrder = 1
    object lblIssuer: TLabel
      Left = 19
      Top = 24
      Width = 34
      Height = 13
      Caption = 'Issuer:'
    end
    object lblDestination: TLabel
      Left = 19
      Top = 56
      Width = 58
      Height = 13
      Caption = 'Destination:'
    end
    object lblService: TLabel
      Left = 19
      Top = 88
      Width = 115
      Height = 13
      Caption = 'Service (e.g. ACS) URL:'
    end
    object editIssuer: TEdit
      Left = 152
      Top = 21
      Width = 313
      Height = 21
      TabOrder = 0
      Text = 'http://saml.localservice.com/metadata/'
    end
    object editDestination: TEdit
      Left = 152
      Top = 54
      Width = 313
      Height = 21
      TabOrder = 1
      Text = 'http://saml.remoteservice.com/sso'
    end
    object editService: TEdit
      Left = 152
      Top = 85
      Width = 313
      Height = 21
      TabOrder = 2
      Text = 'http://saml.localservice.com/acs'
    end
  end
  object gbGenerator: TGroupBox
    Left = 16
    Top = 398
    Width = 569
    Height = 267
    Caption = 'Generate SAML message'
    TabOrder = 2
    object lblIWant: TLabel
      Left = 53
      Top = 28
      Width = 85
      Height = 13
      Caption = 'I want to create: '
    end
    object cbOutputType: TComboBox
      Left = 152
      Top = 25
      Width = 313
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'SP - AuthnRequest'
      Items.Strings = (
        'SP - AuthnRequest'
        'SP - LogoutRequest'
        'SP - AttributeQuery'
        'SP - SubjectQuery'
        'IdP - Assertion'
        'IdP - Enveloped Assertion')
    end
    object btnGo: TButton
      Left = 471
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Go!'
      TabOrder = 1
      OnClick = btnGoClick
    end
    object memoOutput: TMemo
      Left = 19
      Top = 72
      Width = 527
      Height = 177
      ScrollBars = ssVertical
      TabOrder = 2
    end
  end
  object dlgSigningCert: TOpenDialog
    Filter = 
      'PKCS12 certificates (*.pfx)|*.pfx|PEM-encoded certificates (*.pe' +
      'm)|*.pem|DER-encoded certificates (*.cer)|*.cer|All Files (*.*)|' +
      '*.*'
    Left = 512
    Top = 207
  end
  object dlgEncCert: TOpenDialog
    Filter = 
      'PEM-encoded certificates (*.pem)|*.pem|DER-encoded certificates ' +
      '(*.cer)|*.cer|All Files (*.*)|*.*'
    Left = 520
    Top = 343
  end
end


