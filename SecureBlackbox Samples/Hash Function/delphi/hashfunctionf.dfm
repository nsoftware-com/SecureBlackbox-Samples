object FormHashFunction: TFormHashFunction
  Left = 283
  Top = 153
  BorderStyle = bsDialog
  Caption = 'Hash function Demo'
  ClientHeight = 336
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
    Top = 138
    Width = 55
    Height = 13
    Caption = 'Input string:'
  end
  object Label4: TLabel
    Left = 17
    Top = 186
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label2: TLabel
    Left = 17
    Top = 234
    Width = 61
    Height = 13
    Caption = 'Hash output:'
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
    Width = 263
    Height = 13
    Caption = 'This sample shows how to hash message by password.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 17
    Top = 84
    Width = 73
    Height = 13
    Caption = 'Hash algorithm:'
  end
  object edInputStr: TEdit
    Left = 17
    Top = 154
    Width = 321
    Height = 21
    TabOrder = 1
    Text = 'Input String'
  end
  object edHashStr: TEdit
    Left = 17
    Top = 250
    Width = 321
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object bHash: TButton
    Left = 135
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Hash'
    TabOrder = 3
    OnClick = bHashClick
  end
  object edPassword: TEdit
    Left = 17
    Top = 202
    Width = 321
    Height = 21
    TabOrder = 4
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
  object cbHashAlgorithm: TComboBox
    Left = 17
    Top = 103
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 5
    Text = 'SHA256'
    Items.Strings = (
      'SHA1'
      'SHA256'
      'SHA512')
  end
end


