object FormSoapverifier: TFormSoapverifier
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'XML Verifier demo'
  ClientHeight = 140
  ClientWidth = 502
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
  TextHeight = 14
  object lInputFile: TLabel
    Left = 8
    Top = 61
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 416
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Browse...'
    OnClick = sbBrowseInputFileClick
  end
  object lDemoInfo: TLabel
    Left = 5
    Top = 15
    Width = 486
    Height = 28
    AutoSize = False
    Caption = 
      'This sample illustrates processing of SOAP messages with SOAPVer' +
      'ifier control. Please select a signed SOAP message file and clic' +
      'k '#39'Verify'#39'.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object edInputFile: TEdit
    Left = 61
    Top = 58
    Width = 349
    Height = 21
    TabOrder = 0
  end
  object btnVerify: TButton
    Left = 416
    Top = 99
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 1
    OnClick = btnVerifyClick
  end
  object dlgOpen: TOpenDialog
    Left = 284
    Top = 13
  end
end


