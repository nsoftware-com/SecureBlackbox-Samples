object FormWebsocketserver: TFormWebsocketserver
  Left = 0
  Top = 0
  Caption = 'SecureBlackbox WebSocket Server Demo'
  ClientHeight = 481
  ClientWidth = 638
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
    Left = 8
    Top = 15
    Width = 621
    Height = 13
    Caption = 
      'This sample is a basic WebSocket server. Tune up the server comp' +
      'onent as needed, and then press Start to activate the server. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 47
    Width = 622
    Height = 186
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
      Left = 535
      Top = 89
      Width = 70
      Height = 25
      Caption = 'Browse ...'
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
      Width = 505
      Height = 21
      TabOrder = 2
    end
    object edCertPassword: TEdit
      Left = 16
      Top = 147
      Width = 236
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object bbStart: TButton
    Left = 190
    Top = 448
    Width = 70
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = bbStartClick
  end
  object bbStop: TButton
    Left = 334
    Top = 448
    Width = 70
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = bbStopClick
  end
  object mmLog: TMemo
    Left = 8
    Top = 239
    Width = 622
    Height = 194
    Lines.Strings = (
      '')
    TabOrder = 3
  end
  object OpenFileDlg: TOpenDialog
    Left = 408
    Top = 71
  end
end


