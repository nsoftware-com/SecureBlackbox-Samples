object FormServersettings: TFormServersettings
  Left = 615
  Top = 341
  BorderStyle = bsDialog
  Caption = 'Server settings'
  ClientHeight = 213
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbServerSettings: TGroupBox
    Left = 8
    Top = 8
    Width = 409
    Height = 169
    Caption = 'Server settings'
    TabOrder = 0
    object lCertificateFile: TLabel
      Left = 8
      Top = 16
      Width = 260
      Height = 13
      Caption = 'Certificate with private key file (required for TLS/SSL):'
    end
    object lPassword: TLabel
      Left = 312
      Top = 16
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label1: TLabel
      Left = 216
      Top = 72
      Width = 182
      Height = 13
      Caption = 'Host for passive mode (if under NAT):'
    end
    object lPortRange: TLabel
      Left = 216
      Top = 120
      Width = 176
      Height = 13
      Caption = 'Port range for incoming connections:'
    end
    object Label2: TLabel
      Left = 280
      Top = 138
      Width = 4
      Height = 13
      Caption = '-'
    end
    object editCertificateFile: TEdit
      Left = 8
      Top = 32
      Width = 206
      Height = 21
      TabOrder = 0
    end
    object editCertificatePassword: TEdit
      Left = 312
      Top = 32
      Width = 89
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object buttonSelectCertificateFile: TButton
      Left = 220
      Top = 30
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      TabOrder = 2
      OnClick = buttonSelectCertificateFileClick
    end
    object cbImplicitSSL: TCheckBox
      Left = 8
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Implicit SSL'
      TabOrder = 3
    end
    object cbRequireTLS: TCheckBox
      Left = 8
      Top = 120
      Width = 185
      Height = 17
      Caption = 'Require TLS for control channel'
      TabOrder = 4
    end
    object cbRequireTLSForData: TCheckBox
      Left = 8
      Top = 144
      Width = 185
      Height = 17
      Caption = 'Require TLS for data channel'
      TabOrder = 5
    end
    object cbAllowAnonymous: TCheckBox
      Left = 8
      Top = 72
      Width = 145
      Height = 17
      Caption = 'Allow anonymous access'
      TabOrder = 6
    end
    object editPassiveModeHost: TEdit
      Left = 216
      Top = 88
      Width = 185
      Height = 21
      TabOrder = 7
    end
    object editPortsFrom: TEdit
      Left = 216
      Top = 136
      Width = 57
      Height = 21
      TabOrder = 8
      Text = '0'
    end
    object editPortsTo: TEdit
      Left = 296
      Top = 136
      Width = 57
      Height = 21
      TabOrder = 9
      Text = '0'
    end
  end
  object btnOK: TButton
    Left = 168
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object odOpenCertificate: TOpenDialog
    Filter = 'PFX certificates|*.pfx'
    Title = 'Select certificate file:'
    Left = 352
    Top = 184
  end
end
