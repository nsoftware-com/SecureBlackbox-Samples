object FormDcauth: TFormDcauth
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DC Signing Demo'
  ClientHeight = 301
  ClientWidth = 474
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
  object Label10: TLabel
    Left = 72
    Top = 15
    Width = 337
    Height = 13
    Caption = 
      'This sample shows how to sign PDF document using Distributed Cry' +
      'pto'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 19
    Top = 48
    Width = 95
    Height = 13
    Caption = 'Run server on port:'
  end
  object Label2: TLabel
    Left = 19
    Top = 80
    Width = 53
    Height = 13
    Caption = 'Server log:'
  end
  object edPort: TEdit
    Left = 128
    Top = 45
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '10123'
  end
  object mmLog: TMemo
    Left = 19
    Top = 99
    Width = 438
    Height = 150
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object bbStart: TButton
    Left = 152
    Top = 263
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = bbStartClick
  end
  object bbStop: TButton
    Left = 240
    Top = 263
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 3
    OnClick = bbStopClick
  end
end


