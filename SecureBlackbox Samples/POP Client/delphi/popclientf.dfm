object FormPopclient: TFormPopclient
  Left = 449
  Top = 326
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'POP3 Client'
  ClientHeight = 599
  ClientWidth = 824
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
    824
    599)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 629
    Height = 13
    Caption = 
      'This sample shows how to deal with POP3 servers. It can list mes' +
      'sages on a server, receive them from the server and delete them.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object grpServer: TGroupBox
    Left = 8
    Top = 27
    Width = 404
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mail Server (POP3)'
    TabOrder = 0
    object lblHost: TLabel
      Left = 14
      Top = 25
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object lblPort: TLabel
      Left = 302
      Top = 25
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object lblSecurity: TLabel
      Left = 14
      Top = 51
      Width = 92
      Height = 13
      Caption = 'Secure connection:'
    end
    object lblLogin: TLabel
      Left = 35
      Top = 76
      Width = 29
      Height = 13
      Caption = 'Login:'
    end
    object lblPassword: TLabel
      Left = 14
      Top = 103
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object edtHost: TEdit
      Left = 46
      Top = 22
      Width = 243
      Height = 21
      TabOrder = 0
      Text = 'mail.example.com'
    end
    object edtPort: TEdit
      Left = 332
      Top = 22
      Width = 43
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object udPort: TUpDown
      Left = 375
      Top = 22
      Width = 16
      Height = 21
      Associate = edtPort
      Min = 1
      Max = -1
      Position = 1
      TabOrder = 2
    end
    object rbtNoTLS: TRadioButton
      Left = 118
      Top = 50
      Width = 92
      Height = 17
      Caption = 'Don'#39't use TLS'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = rbtNoTLSClick
    end
    object rbtExplicitTLS: TRadioButton
      Left = 216
      Top = 50
      Width = 87
      Height = 17
      Caption = 'Explicit TLS'
      TabOrder = 4
      OnClick = rbtNoTLSClick
    end
    object rbtImplicitTLS: TRadioButton
      Left = 309
      Top = 50
      Width = 82
      Height = 17
      Caption = 'Implicit TLS'
      TabOrder = 5
      OnClick = rbtNoTLSClick
    end
    object edtLogin: TEdit
      Left = 70
      Top = 73
      Width = 321
      Height = 21
      TabOrder = 6
      Text = 'johndoe'
    end
    object edtPassword: TEdit
      Left = 70
      Top = 100
      Width = 321
      Height = 21
      PasswordChar = '*'
      TabOrder = 7
      Text = 'c00lPassw0rD'
    end
    object btnConnect: TButton
      Left = 14
      Top = 132
      Width = 185
      Height = 25
      Caption = 'Connect'
      TabOrder = 8
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 205
      Top = 132
      Width = 186
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 9
      OnClick = btnDisconnectClick
    end
  end
  object grpList: TGroupBox
    Left = 8
    Top = 202
    Width = 404
    Height = 389
    Caption = 'Message List'
    TabOrder = 1
    DesignSize = (
      404
      389)
    object lvwMessages: TListView
      Left = 14
      Top = 24
      Width = 377
      Height = 319
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'UID'
          Width = 250
        end
        item
          Alignment = taRightJustify
          Caption = 'Size'
          Width = 100
        end>
      ColumnClick = False
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object btnDelete: TButton
      Left = 14
      Top = 349
      Width = 185
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      TabOrder = 1
      OnClick = btnDeleteClick
    end
    object btnReceive: TButton
      Left = 205
      Top = 349
      Width = 186
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Receive ->'
      TabOrder = 2
      OnClick = btnReceiveClick
    end
  end
  object grpMessage: TGroupBox
    Left = 418
    Top = 27
    Width = 398
    Height = 564
    Caption = 'Message'
    TabOrder = 2
    object lblFrom: TLabel
      Left = 24
      Top = 25
      Width = 28
      Height = 13
      Caption = 'From:'
    end
    object lblTo: TLabel
      Left = 36
      Top = 52
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object lblSubject: TLabel
      Left = 12
      Top = 106
      Width = 40
      Height = 13
      Caption = 'Subject:'
    end
    object lblDate: TLabel
      Left = 25
      Top = 79
      Width = 27
      Height = 13
      Caption = 'Date:'
    end
    object lblPriority: TLabel
      Left = 271
      Top = 79
      Width = 38
      Height = 13
      Caption = 'Priority:'
    end
    object Label1: TLabel
      Left = 12
      Top = 132
      Width = 59
      Height = 13
      Caption = 'Text (plain):'
    end
    object Label2: TLabel
      Left = 12
      Top = 335
      Width = 63
      Height = 13
      Caption = 'Text (HTML):'
    end
    object lblAttachments: TLabel
      Left = 12
      Top = 534
      Width = 65
      Height = 13
      Caption = 'Attachments:'
    end
    object lblAttachCount: TLabel
      Left = 83
      Top = 534
      Width = 32
      Height = 13
      Caption = '[none]'
    end
    object edtFrom: TEdit
      Left = 58
      Top = 22
      Width = 330
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object edtTo: TEdit
      Left = 58
      Top = 49
      Width = 330
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object edtDate: TEdit
      Left = 58
      Top = 76
      Width = 192
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object edtPriority: TEdit
      Left = 315
      Top = 76
      Width = 73
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object edtSubject: TEdit
      Left = 58
      Top = 103
      Width = 330
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object memTextPlain: TMemo
      Left = 12
      Top = 151
      Width = 376
      Height = 171
      ReadOnly = True
      TabOrder = 5
    end
    object memTextHtml: TMemo
      Left = 12
      Top = 354
      Width = 376
      Height = 170
      ReadOnly = True
      TabOrder = 6
    end
  end
end


