object Formxmlencprops: TFormxmlencprops
  Left = 0
  Top = 0
  Caption = 'Encryption properties'
  ClientHeight = 356
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 14
  object lInfo: TLabel
    Left = 8
    Top = 8
    Width = 359
    Height = 45
    AutoSize = False
    Caption = 
      'The XML document is encrypted. Depending on the document'#39's encry' +
      'ption type you would need to provide a decryption password or de' +
      'cryption certificate.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object gbEncryptionProps: TGroupBox
    Left = 8
    Top = 59
    Width = 377
    Height = 249
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 5
      Top = 101
      Width = 365
      Height = 141
      Caption = 'Key/certificate properties'
      TabOrder = 0
      object lbCertificate: TLabel
        Left = 16
        Top = 64
        Width = 106
        Height = 13
        Caption = 'Encryption certificate:'
        Enabled = False
      end
      object lbCertPassword: TLabel
        Left = 15
        Top = 110
        Width = 103
        Height = 13
        Caption = 'Certificate password:'
        Enabled = False
      end
      object lbPassphrase: TLabel
        Left = 16
        Top = 30
        Width = 72
        Height = 13
        Caption = 'Key/Password:'
      end
      object edPassphrase: TEdit
        Left = 223
        Top = 24
        Width = 125
        Height = 21
        PasswordChar = '*'
        TabOrder = 0
      end
      object edCert: TEdit
        Left = 15
        Top = 83
        Width = 252
        Height = 21
        Enabled = False
        TabOrder = 1
      end
      object btnBrowseCert: TButton
        Left = 273
        Top = 77
        Width = 75
        Height = 25
        Caption = 'Browse...'
        Enabled = False
        TabOrder = 2
        OnClick = btnBrowseCertClick
      end
      object edCertPassword: TEdit
        Left = 223
        Top = 108
        Width = 125
        Height = 21
        Enabled = False
        PasswordChar = '*'
        TabOrder = 3
      end
    end
    object mmInfo: TMemo
      Left = 3
      Top = 16
      Width = 367
      Height = 79
      Color = clBtnFace
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 140
    Top = 319
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object OpenDialogCert: TOpenDialog
    InitialDir = '.'
    Left = 296
    Top = 208
  end
end
