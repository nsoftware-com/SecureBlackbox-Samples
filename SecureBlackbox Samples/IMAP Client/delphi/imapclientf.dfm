object FormImapClient: TFormImapClient
  Left = 316
  Top = 236
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IMAP Client '
  ClientHeight = 605
  ClientWidth = 1104
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1104
    605)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 1088
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample how to deal with IMAP4 servers. It can list mailboxe' +
      's on the server, list messages in the selected mailbox, download' +
      ' them from the server and upload local messages to the server.'
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
    Width = 414
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mail Server (IMAP4)'
    TabOrder = 0
    DesignSize = (
      414
      169)
    object lblHost: TLabel
      Left = 14
      Top = 25
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object lblPort: TLabel
      Left = 312
      Top = 25
      Width = 24
      Height = 13
      Anchors = [akTop, akRight]
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
      Width = 253
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'mail.example.com'
    end
    object edtPort: TEdit
      Left = 342
      Top = 22
      Width = 43
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = '1'
    end
    object udPort: TUpDown
      Left = 385
      Top = 22
      Width = 16
      Height = 21
      Anchors = [akTop, akRight]
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
      Width = 331
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      Text = 'johndow'
    end
    object edtPassword: TEdit
      Left = 70
      Top = 100
      Width = 331
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      PasswordChar = '*'
      TabOrder = 7
      Text = 'c00lPassw0rD'
    end
    object btnConnect: TButton
      Left = 14
      Top = 132
      Width = 185
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Connect'
      TabOrder = 8
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 205
      Top = 132
      Width = 186
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Disconnect'
      TabOrder = 9
      OnClick = btnDisconnectClick
    end
  end
  object grpMailboxes: TGroupBox
    Left = 8
    Top = 202
    Width = 414
    Height = 277
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Mailboxes '
    TabOrder = 1
    DesignSize = (
      414
      277)
    object btnExamine: TButton
      Left = 14
      Top = 241
      Width = 90
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Examine'
      TabOrder = 1
      OnClick = btnExamineClick
    end
    object btnSelect: TButton
      Left = 110
      Top = 241
      Width = 90
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Select'
      TabOrder = 2
      OnClick = btnSelectClick
    end
    object lvwMailboxes: TListView
      Left = 14
      Top = 21
      Width = 387
      Height = 214
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          Alignment = taCenter
          Caption = 'Children'
          Width = 55
        end
        item
          Alignment = taCenter
          Caption = 'Mark'
          Width = 40
        end
        item
          Alignment = taCenter
          Caption = 'Selection'
          Width = 55
        end
        item
          Alignment = taCenter
          Caption = 'Inferiors'
          Width = 55
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      SortType = stText
      TabOrder = 0
      ViewStyle = vsReport
    end
    object btnCreate: TButton
      Left = 206
      Top = 241
      Width = 90
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Create...'
      TabOrder = 3
      OnClick = btnCreateClick
    end
    object btnDelete: TButton
      Left = 302
      Top = 241
      Width = 90
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      TabOrder = 4
      OnClick = btnDeleteClick
    end
  end
  object grpCurrentMailbox: TGroupBox
    Left = 8
    Top = 485
    Width = 414
    Height = 112
    Anchors = [akLeft, akRight, akBottom]
    Caption = ' Current Mailbox '
    TabOrder = 2
    DesignSize = (
      414
      112)
    object lblNameTitle: TLabel
      Left = 14
      Top = 24
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object lblName: TLabel
      Left = 51
      Top = 24
      Width = 350
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '[no current mailbox]'
    end
    object lblTotalTitle: TLabel
      Left = 14
      Top = 48
      Width = 78
      Height = 13
      Caption = 'Total Messages:'
    end
    object lblUnseenTitle: TLabel
      Left = 14
      Top = 67
      Width = 90
      Height = 13
      Caption = 'Unseen Messages:'
    end
    object lblRecentTitle: TLabel
      Left = 14
      Top = 86
      Width = 88
      Height = 13
      Caption = 'Recent Messages:'
    end
    object lblTotal: TLabel
      Left = 110
      Top = 48
      Width = 90
      Height = 13
      AutoSize = False
      Caption = '-'
    end
    object lblUnseen: TLabel
      Left = 110
      Top = 67
      Width = 90
      Height = 13
      AutoSize = False
      Caption = '-'
    end
    object lblRecent: TLabel
      Left = 111
      Top = 86
      Width = 89
      Height = 13
      AutoSize = False
      Caption = '-'
    end
    object lblReadOnly: TLabel
      Left = 321
      Top = 48
      Width = 75
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
    end
    object btnRefresh: TButton
      Left = 321
      Top = 75
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btnRefreshClick
    end
  end
  object grpMessages: TGroupBox
    Left = 428
    Top = 27
    Width = 668
    Height = 570
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Messages '
    Constraints.MinWidth = 546
    TabOrder = 3
    DesignSize = (
      668
      570)
    object lblFilename: TLabel
      Left = 12
      Top = 536
      Width = 46
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Filename:'
    end
    object btnListAll: TButton
      Left = 12
      Top = 20
      Width = 100
      Height = 25
      Caption = 'List All'
      TabOrder = 0
      OnClick = btnListAllClick
    end
    object btnListNew: TButton
      Left = 118
      Top = 20
      Width = 100
      Height = 25
      Caption = 'List New'
      TabOrder = 1
      OnClick = btnListNewClick
    end
    object btnListRecent: TButton
      Left = 224
      Top = 20
      Width = 100
      Height = 25
      Caption = 'List Recent'
      TabOrder = 2
      OnClick = btnListRecentClick
    end
    object btnListUnseen: TButton
      Left = 330
      Top = 20
      Width = 100
      Height = 25
      Caption = 'List Unseen'
      TabOrder = 3
      OnClick = btnListUnseenClick
    end
    object btnListDeleted: TButton
      Left = 436
      Top = 20
      Width = 100
      Height = 25
      Caption = 'List Deleted'
      TabOrder = 4
      OnClick = btnListDeletedClick
    end
    object lvwMessages: TListView
      Left = 12
      Top = 54
      Width = 643
      Height = 436
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'From'
          Width = 120
        end
        item
          Caption = 'To'
          Width = 120
        end
        item
          Caption = 'Subject'
          Width = 210
        end
        item
          Caption = 'Date'
          Width = 100
        end
        item
          Alignment = taRightJustify
          Caption = 'Size'
          Width = 70
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 5
      ViewStyle = vsReport
    end
    object btnMarkDeleted: TButton
      Left = 9
      Top = 496
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Mark Deleted'
      TabOrder = 6
      OnClick = btnMarkDeletedClick
    end
    object btnPurge: TButton
      Left = 115
      Top = 496
      Width = 164
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Purge Deleted Messages'
      TabOrder = 7
      OnClick = btnPurgeClick
    end
    object edtFilename: TEdit
      Left = 64
      Top = 533
      Width = 591
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 8
    end
    object btnReceive: TButton
      Left = 285
      Top = 496
      Width = 181
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Receive Message to File'
      TabOrder = 9
      OnClick = btnReceiveClick
    end
    object btnPost: TButton
      Left = 472
      Top = 496
      Width = 180
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Post Message from File'
      TabOrder = 10
      OnClick = btnPostClick
    end
  end
end


