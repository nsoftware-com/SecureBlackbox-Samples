object FormJWSigner: TFormJWSigner
  Left = 283
  Top = 153
  BorderStyle = bsDialog
  Caption = 'JWC signing demo'
  ClientHeight = 316
  ClientWidth = 374
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
    Top = 141
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label2: TLabel
    Left = 17
    Top = 226
    Width = 48
    Height = 13
    Caption = 'Signature:'
  end
  object Label10: TLabel
    Left = 8
    Top = 8
    Width = 354
    Height = 13
    Caption = 
      'This sample shows how to create a detached signature over a text' +
      ' string.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblKeyFilename: TLabel
    Left = 17
    Top = 95
    Width = 168
    Height = 13
    Caption = 'Certificate file (private key required):'
  end
  object edInputStr: TEdit
    Left = 17
    Top = 58
    Width = 345
    Height = 21
    TabOrder = 0
    Text = 
      'All we have to decide is what to do with the time that is given ' +
      'us.'
  end
  object edSignatureStr: TEdit
    Left = 17
    Top = 242
    Width = 345
    Height = 21
    TabOrder = 1
  end
  object bSign: TButton
    Left = 99
    Top = 283
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 2
    OnClick = bSignClick
  end
  object bVerify: TButton
    Left = 211
    Top = 283
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 3
    OnClick = bVerifyClick
  end
  object edPassword: TEdit
    Left = 17
    Top = 157
    Width = 176
    Height = 21
    TabOrder = 4
  end
  object cbCompact: TCheckBox
    Left = 17
    Top = 196
    Width = 97
    Height = 17
    Caption = 'Compact Mode'
    TabOrder = 5
  end
  object edCertFile: TEdit
    Left = 17
    Top = 114
    Width = 269
    Height = 21
    TabOrder = 6
  end
  object btnBrowse: TButton
    Left = 292
    Top = 112
    Width = 70
    Height = 25
    Caption = 'Browse...'
    TabOrder = 7
    OnClick = btnBrowseClick
  end
  object dlgOpen: TOpenDialog
    Left = 272
    Top = 192
  end
end


