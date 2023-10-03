object FormMessagedecompressor: TFormMessagedecompressor
  Left = 0
  Top = 0
  Caption = 'Message Decompressor demo'
  ClientHeight = 193
  ClientWidth = 395
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
  object lbInputFile: TLabel
    Left = 8
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 303
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object lOutputFile: TLabel
    Left = 8
    Top = 84
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 303
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 330
    Height = 13
    Caption = 'This sample performs decompress of compressed PKCS#7 messages.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 225
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 80
    Width = 225
    Height = 21
    TabOrder = 1
  end
  object btnDecompress: TButton
    Left = 303
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Decompress'
    TabOrder = 2
    OnClick = btnDecompressClick
  end
  object dlgOpenFile: TOpenDialog
    Left = 112
    Top = 32
  end
  object dlgSaveFile: TSaveDialog
    Left = 216
    Top = 32
  end
end


