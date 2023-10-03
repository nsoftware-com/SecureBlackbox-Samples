object FormMessagetimestampverifier: TFormMessagetimestampverifier
  Left = 0
  Top = 0
  Caption = 'Message Timestamp Verifier demo'
  ClientHeight = 213
  ClientWidth = 557
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
  object lbXMLFile: TLabel
    Left = 8
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 474
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object lOutputFile: TLabel
    Left = 8
    Top = 111
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbBrowseOutputFile: TSpeedButton
    Left = 474
    Top = 105
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseOutputFileClick
  end
  object Label10: TLabel
    Left = 5
    Top = 15
    Width = 529
    Height = 13
    Caption = 
      'This sample showcases MessageTimestampVerifier'#39's facilities in v' +
      'alidating PKCS#7-compliant timestamped files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cbDetached: TCheckBox
    Left = 8
    Top = 84
    Width = 209
    Height = 17
    Caption = 'Detached'
    TabOrder = 0
    OnClick = cbDetachedClick
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 360
    Height = 21
    TabOrder = 1
  end
  object btnVerify: TButton
    Left = 474
    Top = 180
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 2
    OnClick = btnVerifyClick
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 107
    Width = 360
    Height = 21
    TabOrder = 3
  end
  object dlgOpen: TOpenDialog
    Left = 241
    Top = 32
  end
  object dlgSave: TSaveDialog
    Left = 304
    Top = 32
  end
end


