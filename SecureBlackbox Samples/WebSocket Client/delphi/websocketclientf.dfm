object FormWebsocketclient: TFormWebsocketclient
  Left = 346
  Top = 177
  BorderStyle = bsDialog
  Caption = 'Websocket Client demo'
  ClientHeight = 430
  ClientWidth = 433
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
    Width = 408
    Height = 13
    Caption = 
      'This sample illustrates the use of WebsocketClient component (ru' +
      'n with server demo)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnConnect: TButton
    Left = 139
    Top = 135
    Width = 75
    Height = 25
    Caption = 'Connect'
    Default = True
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object MMLog: TMemo
    Left = 0
    Top = 211
    Width = 433
    Height = 219
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
    ExplicitTop = 224
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 417
    Height = 89
    Caption = 'Http options  '
    TabOrder = 0
    object Label1: TLabel
      Left = 15
      Top = 50
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object Label2: TLabel
      Left = 15
      Top = 25
      Width = 43
      Height = 13
      Caption = 'Protocol:'
    end
    object Label3: TLabel
      Left = 212
      Top = 50
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object EdHost: TEdit
      Left = 74
      Top = 46
      Width = 129
      Height = 21
      TabOrder = 1
      Text = 'localhost'
    end
    object CbProtocol: TComboBox
      Left = 74
      Top = 21
      Width = 95
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'HTTP'
        'HTTPS')
    end
    object EdPort: TEdit
      Left = 240
      Top = 46
      Width = 91
      Height = 21
      TabOrder = 2
      Text = '8080'
    end
  end
  object editText: TEdit
    Left = 8
    Top = 184
    Width = 336
    Height = 21
    TabOrder = 3
    Text = 'Hello!'
  end
  object btnSend: TButton
    Left = 350
    Top = 180
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 4
    OnClick = btnSendClick
  end
  object btnDisconnect: TButton
    Left = 220
    Top = 135
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 2
    OnClick = btnDisconnectClick
  end
  object dlgSave: TSaveDialog
    Left = 466
    Top = 8
  end
end


