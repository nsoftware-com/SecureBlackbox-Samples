object FormDtlsClient: TFormDtlsClient
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DTLS Client Demo'
  ClientHeight = 392
  ClientWidth = 837
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    837
    392)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 821
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample is a very simple program that shows how to use the D' +
      'TLSClient component to send and receive messages to and from a D' +
      'TLS server.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 805
  end
  object grpServer: TGroupBox
    Left = 8
    Top = 27
    Width = 299
    Height = 85
    Caption = ' Server '
    TabOrder = 0
    DesignSize = (
      299
      85)
    object lblHost: TLabel
      Left = 13
      Top = 22
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object lblPort: TLabel
      Left = 15
      Top = 49
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object edtHost: TEdit
      Left = 45
      Top = 19
      Width = 241
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'localhost'
    end
    object edtPort: TEdit
      Left = 45
      Top = 46
      Width = 40
      Height = 21
      TabOrder = 1
      Text = '4433'
    end
    object udPort: TUpDown
      Left = 85
      Top = 46
      Width = 17
      Height = 21
      Associate = edtPort
      Min = 1
      Max = 65535
      Position = 4433
      TabOrder = 2
      Thousands = False
    end
    object btnConnect: TButton
      Left = 130
      Top = 46
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Connect'
      TabOrder = 3
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 211
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 4
      OnClick = btnDisconnectClick
    end
  end
  object grpMessage: TGroupBox
    Left = 8
    Top = 118
    Width = 299
    Height = 266
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Message '
    TabOrder = 1
    DesignSize = (
      299
      266)
    object lblText: TLabel
      Left = 13
      Top = 21
      Width = 26
      Height = 13
      Caption = 'Text:'
    end
    object memText: TMemo
      Left = 13
      Top = 40
      Width = 273
      Height = 184
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
    end
    object btnSend: TButton
      Left = 211
      Top = 230
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Send'
      Enabled = False
      TabOrder = 1
      OnClick = btnSendClick
    end
  end
  object grpLog: TGroupBox
    Left = 313
    Top = 27
    Width = 516
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Log '
    TabOrder = 2
    DesignSize = (
      516
      357)
    object lvwAllMessages: TListView
      Left = 12
      Top = 19
      Width = 491
      Height = 327
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Time'
          Width = 60
        end
        item
          AutoSize = True
          Caption = 'Event'
        end>
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
end


