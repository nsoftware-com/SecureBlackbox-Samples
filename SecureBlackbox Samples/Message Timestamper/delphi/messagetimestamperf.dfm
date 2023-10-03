object FormMessagetimestamper: TFormMessagetimestamper
  Left = 0
  Top = 0
  Caption = 'Message Timestamper demo'
  ClientHeight = 280
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
    Width = 337
    Height = 13
    Caption = 
      'This sample illustrates how to create timestamped PKCS#7 message' +
      's. '
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
  object btnTimestamp: TButton
    Left = 323
    Top = 244
    Width = 75
    Height = 25
    Caption = 'Timestamp'
    TabOrder = 2
    OnClick = btnTimestampClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 120
    Width = 390
    Height = 105
    Caption = 'Timestamping options  '
    TabOrder = 3
    object Label2: TLabel
      Left = 24
      Top = 72
      Width = 89
      Height = 13
      Caption = 'Timestamp server:'
    end
    object cbDetached: TCheckBox
      Left = 24
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Detached'
      TabOrder = 0
    end
    object edTimestampServer: TEdit
      Left = 128
      Top = 68
      Width = 193
      Height = 21
      TabOrder = 1
    end
  end
  object dlgOpenFile: TOpenDialog
    Left = 352
    Top = 128
  end
  object dlgSaveFile: TSaveDialog
    Left = 352
    Top = 176
  end
end


