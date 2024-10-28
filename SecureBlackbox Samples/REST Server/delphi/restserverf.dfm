object FormRestserver: TFormRestserver
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SecureBlackbox REST Server Demo'
  ClientHeight = 606
  ClientWidth = 467
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
  object Label2: TLabel
    Left = 8
    Top = 271
    Width = 426
    Height = 13
    Caption = 
      'Items stored on this server. These items can be managed via brow' +
      'ser after server start:'
  end
  object Label10: TLabel
    Left = 8
    Top = 8
    Width = 378
    Height = 13
    Caption = 
      'This sample is a basic REST server. Tune up the server component' +
      ' as needed, '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 27
    Width = 214
    Height = 13
    Caption = 'and then press Start to activate the server. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 9
    Top = 55
    Width = 449
    Height = 194
    Caption = 'Server Options'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 32
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object Label3: TLabel
      Left = 16
      Top = 72
      Width = 83
      Height = 13
      Caption = 'X.509 certificate:'
    end
    object SpeedButton1: TSpeedButton
      Left = 407
      Top = 90
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object Label4: TLabel
      Left = 16
      Top = 128
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object edPort: TEdit
      Left = 62
      Top = 29
      Width = 163
      Height = 21
      TabOrder = 0
      Text = '8080'
    end
    object cbUseTLS: TCheckBox
      Left = 240
      Top = 31
      Width = 97
      Height = 17
      Caption = 'Use SSL/TLS'
      TabOrder = 1
    end
    object edCertFile: TEdit
      Left = 16
      Top = 91
      Width = 385
      Height = 21
      TabOrder = 2
    end
    object edCertPassword: TEdit
      Left = 16
      Top = 147
      Width = 385
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object bbStart: TButton
    Left = 150
    Top = 573
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = bbStartClick
  end
  object bbStop: TButton
    Left = 231
    Top = 573
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = bbStopClick
  end
  object mmLog: TMemo
    Left = 8
    Top = 432
    Width = 451
    Height = 129
    Lines.Strings = (
      '')
    TabOrder = 3
  end
  object lvItems: TListView
    Left = 9
    Top = 296
    Width = 449
    Height = 121
    Columns = <
      item
        Caption = 'ID'
      end
      item
        Caption = 'Text'
      end>
    TabOrder = 4
    ViewStyle = vsReport
  end
  object OpenFileDlg: TOpenDialog
    Left = 408
    Top = 31
  end
end


