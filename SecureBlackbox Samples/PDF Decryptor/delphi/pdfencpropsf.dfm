object Formpdfencprops: TFormpdfencprops
  Left = 0
  Top = 0
  Caption = 'Encryption properties'
  ClientHeight = 356
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lInfo: TLabel
    Left = 8
    Top = 8
    Width = 359
    Height = 45
    AutoSize = False
    Caption = 
      'The PDF document is encrypted. Depending on the document'#39's encry' +
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
    Width = 361
    Height = 249
    TabOrder = 0
    object lCertificate: TLabel
      Left = 38
      Top = 149
      Width = 107
      Height = 13
      Caption = 'Decryption certificate:'
      Enabled = False
    end
    object lCertPassword: TLabel
      Left = 38
      Top = 197
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
      Enabled = False
    end
    object lProvideCertificate: TLabel
      Left = 16
      Top = 130
      Width = 162
      Height = 13
      Caption = 'Provide certificate for decryption:'
      Enabled = False
    end
    object lProvidePassword: TLabel
      Left = 16
      Top = 74
      Width = 160
      Height = 13
      Caption = 'Provide password for decryption:'
      Enabled = False
    end
    object lEncryptionAlgorithm: TLabel
      Left = 16
      Top = 18
      Width = 102
      Height = 13
      Caption = 'Encryption algorithm:'
    end
    object lMetadataStatus: TLabel
      Left = 16
      Top = 43
      Width = 83
      Height = 13
      Caption = 'Metadata status:'
    end
    object editPassword: TEdit
      Left = 38
      Top = 103
      Width = 129
      Height = 21
      PasswordChar = '*'
      TabOrder = 0
    end
    object editCert: TEdit
      Left = 38
      Top = 165
      Width = 225
      Height = 21
      Enabled = False
      TabOrder = 1
    end
    object btnBrowseCert: TButton
      Left = 270
      Top = 165
      Width = 75
      Height = 25
      Caption = 'Browse...'
      Enabled = False
      TabOrder = 2
      OnClick = btnBrowseCertClick
    end
    object editCertPassword: TEdit
      Left = 38
      Top = 213
      Width = 129
      Height = 21
      Enabled = False
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object btnOK: TButton
    Left = 109
    Top = 319
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 189
    Top = 319
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OpenDialogCert: TOpenDialog
    InitialDir = '.'
    Left = 296
    Top = 208
  end
end
