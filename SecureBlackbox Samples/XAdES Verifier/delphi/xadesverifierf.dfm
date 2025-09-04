object FormXadesverifier: TFormXadesverifier
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'XAdES Verifier demo'
  ClientHeight = 184
  ClientWidth = 432
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
    Left = 350
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object lDataFile: TLabel
    Left = 8
    Top = 111
    Width = 44
    Height = 13
    Caption = 'Data file:'
  end
  object sbBrowseDataFile: TSpeedButton
    Left = 350
    Top = 105
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    Enabled = False
    OnClick = sbBrowseDataFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 8
    Width = 396
    Height = 24
    AutoSize = False
    Caption = 
      'This sample shows processing of XAdES signatures. Please select ' +
      'a signed XML file and click '#39'Verify'#39' when ready.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
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
    Left = 65
    Top = 45
    Width = 279
    Height = 21
    TabOrder = 1
  end
  object btnVerify: TButton
    Left = 350
    Top = 151
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 2
    OnClick = btnVerifyClick
  end
  object edDataFile: TEdit
    Left = 65
    Top = 107
    Width = 279
    Height = 21
    Enabled = False
    TabOrder = 3
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 225
    Top = 64
  end
  object FVerifier: TsbxXAdESVerifier
    Left = 288
    Top = 64
  end
end


