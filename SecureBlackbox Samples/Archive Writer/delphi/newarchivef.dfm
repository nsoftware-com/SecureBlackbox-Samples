object FormNewarchive: TFormNewarchive
  Left = 799
  Top = 216
  BorderStyle = bsSingle
  Caption = 'Archive options'
  ClientHeight = 261
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblArchiveName: TLabel
    Left = 8
    Top = 11
    Width = 55
    Height = 13
    Caption = 'Archive file:'
  end
  object edtArchiveFile: TEdit
    Left = 79
    Top = 7
    Width = 340
    Height = 21
    TabOrder = 0
  end
  object btnChoose: TButton
    Left = 425
    Top = 5
    Width = 80
    Height = 25
    Caption = 'Choose...'
    TabOrder = 1
    OnClick = btnChooseClick
  end
  object gbCompressionOptions: TGroupBox
    Left = 8
    Top = 45
    Width = 249
    Height = 105
    Caption = 'Compression options:'
    TabOrder = 2
    object lblCompressionLevel: TLabel
      Left = 10
      Top = 64
      Width = 88
      Height = 13
      Caption = 'Compression level:'
    end
    object lblArchiveType: TLabel
      Left = 36
      Top = 32
      Width = 62
      Height = 13
      Caption = 'Archive type:'
    end
    object cbCompressionLevel: TComboBox
      Left = 112
      Top = 61
      Width = 89
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'Fastest'
        'Low'
        'Normal'
        'High')
    end
    object cbArchiveType: TComboBox
      Left = 112
      Top = 29
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Zip'
      OnChange = cbArchiveTypeChange
      Items.Strings = (
        'Zip'
        'Tar Gzip'
        'Tar Bzip2'
        'Gzip'
        'Bzip2')
    end
  end
  object btnCancel: TButton
    Left = 424
    Top = 225
    Width = 80
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnOk: TButton
    Left = 320
    Top = 225
    Width = 80
    Height = 25
    Caption = 'OK'
    TabOrder = 4
    OnClick = btnOkClick
  end
  object gbSecurityOptions: TGroupBox
    Left = 271
    Top = 45
    Width = 233
    Height = 164
    Caption = 'Security:'
    TabOrder = 5
    object lblPassword: TLabel
      Left = 8
      Top = 86
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object lblPasswordConfirmation: TLabel
      Left = 8
      Top = 113
      Width = 61
      Height = 13
      Caption = 'Confirmation:'
    end
    object lblEncryption: TLabel
      Left = 8
      Top = 24
      Width = 53
      Height = 13
      Caption = 'Encryption:'
    end
    object lblEncryptionAlgorithm: TLabel
      Left = 8
      Top = 54
      Width = 98
      Height = 13
      Caption = 'Encryption algorithm:'
    end
    object cbEncryptionAlgorithm: TComboBox
      Left = 136
      Top = 48
      Width = 89
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'AES128'
        'AES192'
        'AES256'
        'Blowfish'
        'Twofish')
    end
    object edtPassword: TEdit
      Left = 80
      Top = 83
      Width = 145
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
      Text = 'password'
    end
    object edtPasswordConfirmation: TEdit
      Left = 80
      Top = 110
      Width = 145
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object cbShowPassword: TCheckBox
      Left = 8
      Top = 135
      Width = 121
      Height = 17
      Caption = 'Show password'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbShowPasswordClick
    end
    object cbEncryptionType: TComboBox
      Left = 72
      Top = 24
      Width = 153
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnChange = cbEncryptionTypeChange
      Items.Strings = (
        'None'
        'Generic'
        'WinZip'
        'Strong')
    end
  end
  object sdSaveArchive: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip archives|*.zip|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Select archive file name:'
    Left = 144
    Top = 176
  end
end
