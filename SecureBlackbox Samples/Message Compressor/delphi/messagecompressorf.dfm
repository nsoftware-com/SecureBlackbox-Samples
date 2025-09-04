object FormMessagecompressor: TFormMessagecompressor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Message '#1057'ompressor demo'
  ClientHeight = 196
  ClientWidth = 404
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
    Left = 323
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object Label1: TLabel
    Left = 8
    Top = 84
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 323
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 333
    Height = 13
    Caption = 
      'This sample illustrates how to create compressed PKCS#7 messages' +
      '. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lbSymmetricAlgorithm: TLabel
    Left = 8
    Top = 127
    Width = 90
    Height = 13
    Caption = 'Compression level:'
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 245
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 80
    Width = 245
    Height = 21
    TabOrder = 1
  end
  object btnCompress: TButton
    Left = 323
    Top = 164
    Width = 75
    Height = 25
    Caption = 'Compress'
    TabOrder = 2
    OnClick = btnCompressClick
  end
  object cbCompressionLevel: TComboBox
    Left = 114
    Top = 124
    Width = 103
    Height = 21
    Style = csDropDownList
    ItemIndex = 5
    TabOrder = 3
    Text = '6'
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9')
  end
  object dlgOpenFile: TOpenDialog
    Left = 352
    Top = 112
  end
  object dlgSaveFile: TSaveDialog
    Left = 288
    Top = 112
  end
end


