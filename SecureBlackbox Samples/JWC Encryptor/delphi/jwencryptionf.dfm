object FormJWEncryption: TFormJWEncryption
  Left = 283
  Top = 153
  BorderStyle = bsDialog
  Caption = 'JWC encryption demo'
  ClientHeight = 311
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
    Top = 42
    Width = 71
    Height = 13
    Caption = 'Plaintext string:'
  end
  object Label4: TLabel
    Left = 17
    Top = 90
    Width = 101
    Height = 13
    Caption = 'Encryption password:'
  end
  object Label5: TLabel
    Left = 17
    Top = 218
    Width = 80
    Height = 13
    Caption = 'Decrypted string:'
  end
  object Label2: TLabel
    Left = 17
    Top = 170
    Width = 81
    Height = 13
    Caption = 'Encrypted token:'
  end
  object Label10: TLabel
    Left = 8
    Top = 8
    Width = 342
    Height = 13
    Caption = 
      'This sample shows how to encrypt text to a JW token with a passw' +
      'ord.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputStr: TEdit
    Left = 17
    Top = 58
    Width = 321
    Height = 21
    TabOrder = 0
    Text = 'And now that you don’t have to be perfect, you can be good.'
  end
  object edEncryptedStr: TEdit
    Left = 17
    Top = 186
    Width = 321
    Height = 21
    TabOrder = 1
  end
  object bbEncrypt: TButton
    Left = 78
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    TabOrder = 2
    OnClick = bbEncryptClick
  end
  object bbDecrypt: TButton
    Left = 182
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Decrypt'
    TabOrder = 3
    OnClick = bbDecryptClick
  end
  object edDecryptedStr: TEdit
    Left = 17
    Top = 234
    Width = 321
    Height = 21
    ReadOnly = True
    TabOrder = 4
  end
  object edPassword: TEdit
    Left = 17
    Top = 106
    Width = 160
    Height = 21
    TabOrder = 5
  end
  object cbCompact: TCheckBox
    Left = 17
    Top = 140
    Width = 97
    Height = 17
    Caption = 'Compact Mode'
    TabOrder = 6
  end
end


