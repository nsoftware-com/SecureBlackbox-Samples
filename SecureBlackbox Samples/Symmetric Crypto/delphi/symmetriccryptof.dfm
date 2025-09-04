object FormSymmetricCrypto: TFormSymmetricCrypto
  Left = 283
  Top = 153
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Symmetric crypto Demo'
  ClientHeight = 329
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 17
    Top = 90
    Width = 55
    Height = 13
    Caption = 'Input string:'
  end
  object Label4: TLabel
    Left = 17
    Top = 138
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label5: TLabel
    Left = 17
    Top = 234
    Width = 80
    Height = 13
    Caption = 'Decrypted string:'
  end
  object Label2: TLabel
    Left = 17
    Top = 186
    Width = 84
    Height = 13
    Caption = 'Encrypted output:'
  end
  object Label3: TLabel
    Left = 17
    Top = 36
    Width = 48
    Height = 13
    Caption = 'Encoding:'
  end
  object Label10: TLabel
    Left = 8
    Top = 8
    Width = 338
    Height = 13
    Caption = 
      'This sample shows how to encrypt and decrypt message by password' +
      '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputStr: TEdit
    Left = 17
    Top = 106
    Width = 321
    Height = 21
    TabOrder = 1
    Text = 'Input String'
  end
  object edEncryptedStr: TEdit
    Left = 17
    Top = 202
    Width = 321
    Height = 21
    TabOrder = 2
  end
  object bbEncrypt: TButton
    Left = 87
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    TabOrder = 3
    OnClick = bbEncryptClick
  end
  object bbDecrypt: TButton
    Left = 191
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Decrypt'
    TabOrder = 4
    OnClick = bbDecryptClick
  end
  object edDecryptedStr: TEdit
    Left = 17
    Top = 250
    Width = 321
    Height = 21
    ReadOnly = True
    TabOrder = 5
  end
  object edPassword: TEdit
    Left = 17
    Top = 154
    Width = 321
    Height = 21
    TabOrder = 6
  end
  object cbEncoding: TComboBox
    Left = 17
    Top = 55
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 0
    Text = 'Base64'
    Items.Strings = (
      'Binary'
      'Base64'
      'Compact'
      'JSON')
  end
end


