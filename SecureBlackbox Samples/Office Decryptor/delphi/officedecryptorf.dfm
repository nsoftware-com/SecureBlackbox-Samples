object FormOfficedecryptor: TFormOfficedecryptor
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Office Decryptor demo'
  ClientHeight = 178
  ClientWidth = 331
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
  object lSourceFile: TLabel
    Left = 7
    Top = 56
    Width = 86
    Height = 13
    Caption = 'Source Office file:'
  end
  object lDestFile: TLabel
    Left = 7
    Top = 98
    Width = 107
    Height = 13
    Caption = 'Destination Office file:'
  end
  object lDemoInfo: TLabel
    Left = 6
    Top = 8
    Width = 312
    Height = 42
    AutoSize = False
    Caption = 
      'This sample illustrates the use of OfficeDecryptor component for' +
      ' decrypting Office documents. Please pick the Office document an' +
      'd then click '#39'Decrypt'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object edSource: TEdit
    Left = 7
    Top = 70
    Width = 242
    Height = 21
    TabOrder = 0
  end
  object edDest: TEdit
    Left = 7
    Top = 111
    Width = 242
    Height = 21
    TabOrder = 2
  end
  object btnBrowseSource: TButton
    Left = 255
    Top = 68
    Width = 70
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseSourceClick
  end
  object btnBrowseDest: TButton
    Left = 255
    Top = 109
    Width = 70
    Height = 25
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btnBrowseDestClick
  end
  object btnDecrypt: TButton
    Left = 98
    Top = 145
    Width = 70
    Height = 25
    Caption = 'Decrypt'
    Default = True
    TabOrder = 4
    OnClick = btnDecryptClick
  end
  object btnCancel: TButton
    Left = 174
    Top = 145
    Width = 70
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object dlgOpen: TOpenDialog
    InitialDir = '.'
    Left = 272
    Top = 80
  end
  object dlgSave: TSaveDialog
    InitialDir = '.'
    Left = 272
    Top = 128
  end
end


