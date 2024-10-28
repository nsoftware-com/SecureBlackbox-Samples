object FormTlsclient: TFormTlsclient
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SecureBlackbox TLS Client Demo'
  ClientHeight = 372
  ClientWidth = 439
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
  object Label3: TLabel
    Left = 8
    Top = 344
    Width = 69
    Height = 13
    Caption = 'Message text:'
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 240
    Height = 13
    Caption = 'This sample illustrates basic TLS client operations. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 425
    Height = 121
    Caption = 'Server'
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 24
      Width = 88
      Height = 13
      Caption = 'Server IP/Domain:'
    end
    object Label2: TLabel
      Left = 310
      Top = 24
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object edAddress: TEdit
      Left = 24
      Top = 43
      Width = 271
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object edPort: TEdit
      Left = 310
      Top = 43
      Width = 82
      Height = 21
      TabOrder = 1
      Text = '8080'
    end
    object btnDisconnect: TButton
      Left = 317
      Top = 81
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = btnDisconnectClick
    end
    object cbUseTLS: TCheckBox
      Left = 24
      Top = 85
      Width = 97
      Height = 17
      Caption = 'Use SSL/TLS'
      TabOrder = 3
    end
  end
  object edMsg: TEdit
    Left = 83
    Top = 341
    Width = 270
    Height = 21
    TabOrder = 1
    Text = 'Hello server!'
  end
  object mmMsg: TMemo
    Left = 8
    Top = 175
    Width = 425
    Height = 154
    TabOrder = 2
  end
  object btnConnect: TButton
    Left = 244
    Top = 129
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object btnSend: TButton
    Left = 359
    Top = 339
    Width = 75
    Height = 25
    Caption = 'Send'
    Enabled = False
    TabOrder = 4
    OnClick = btnSendClick
  end
end


