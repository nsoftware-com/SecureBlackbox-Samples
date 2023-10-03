object FormSmtpclient: TFormSmtpclient
  Left = 506
  Top = 277
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SMTP Client'
  ClientHeight = 688
  ClientWidth = 513
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
  DesignSize = (
    513
    688)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 497
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample shows how to prepare and send a mail message quickly' +
      ' using the SMTPClient component.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object grpServer: TGroupBox
    Left = 8
    Top = 27
    Width = 497
    Height = 139
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mail Server (SMTP)'
    TabOrder = 0
    DesignSize = (
      497
      139)
    object lblHost: TLabel
      Left = 14
      Top = 25
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object lblPort: TLabel
      Left = 397
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
      Left = 14
      Top = 76
      Width = 85
      Height = 13
      Caption = 'Login (if needed):'
    end
    object lblPassword: TLabel
      Left = 49
      Top = 102
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object edtHost: TEdit
      Left = 46
      Top = 22
      Width = 332
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'mail.example.com'
    end
    object edtPort: TEdit
      Left = 427
      Top = 22
      Width = 43
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = '1'
    end
    object udPort: TUpDown
      Left = 470
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
      Left = 105
      Top = 73
      Width = 381
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      Text = 'johndoe'
    end
    object edtPassword: TEdit
      Left = 105
      Top = 100
      Width = 381
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      PasswordChar = '*'
      TabOrder = 7
      Text = 'c00lPassw0rD'
    end
  end
  object grpMessage: TGroupBox
    Left = 8
    Top = 172
    Width = 497
    Height = 477
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Mail Message'
    TabOrder = 1
    DesignSize = (
      497
      477)
    object lblFrom: TLabel
      Left = 26
      Top = 30
      Width = 28
      Height = 13
      Caption = 'From:'
    end
    object lblNameHint: TLabel
      Left = 136
      Top = 50
      Width = 34
      Height = 13
      Caption = '(name)'
    end
    object lblAddressHint: TLabel
      Left = 348
      Top = 50
      Width = 46
      Height = 13
      Caption = '(address)'
    end
    object lblSubject: TLabel
      Left = 14
      Top = 105
      Width = 40
      Height = 13
      Caption = 'Subject:'
    end
    object lblTo: TLabel
      Left = 38
      Top = 70
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object lblText: TLabel
      Left = 16
      Top = 162
      Width = 59
      Height = 13
      Caption = 'Text (plain):'
    end
    object lblPriority: TLabel
      Left = 16
      Top = 136
      Width = 38
      Height = 13
      Caption = 'Priority:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edtFromName: TEdit
      Left = 60
      Top = 27
      Width = 211
      Height = 21
      TabOrder = 0
      Text = 'John Doe'
    end
    object edtFromAddress: TEdit
      Left = 277
      Top = 27
      Width = 209
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'john.doe@example.com'
    end
    object edtToName: TEdit
      Left = 60
      Top = 67
      Width = 211
      Height = 21
      TabOrder = 2
      Text = 'Jane Doe'
    end
    object edtToAddress: TEdit
      Left = 277
      Top = 67
      Width = 210
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = 'jane.doe@example.com'
    end
    object edtSubject: TEdit
      Left = 60
      Top = 102
      Width = 427
      Height = 21
      TabOrder = 4
      Text = 'Good news'
    end
    object rbtLowest: TRadioButton
      Left = 60
      Top = 135
      Width = 57
      Height = 17
      Caption = 'Lowest'
      TabOrder = 5
    end
    object rbtLow: TRadioButton
      Left = 123
      Top = 135
      Width = 43
      Height = 17
      Caption = 'Low'
      TabOrder = 6
    end
    object rbtNormal: TRadioButton
      Left = 172
      Top = 135
      Width = 55
      Height = 17
      Caption = 'Normal'
      Checked = True
      TabOrder = 7
      TabStop = True
    end
    object rbtHigh: TRadioButton
      Left = 233
      Top = 135
      Width = 44
      Height = 17
      Caption = 'High'
      TabOrder = 8
    end
    object rbtHighest: TRadioButton
      Left = 283
      Top = 135
      Width = 60
      Height = 17
      Caption = 'Highest'
      TabOrder = 9
    end
    object memText: TMemo
      Left = 14
      Top = 181
      Width = 473
      Height = 282
      Lines.Strings = (
        '... message text goes here ...')
      TabOrder = 10
    end
  end
  object btnSend: TButton
    Left = 8
    Top = 655
    Width = 497
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Send Message'
    TabOrder = 2
    OnClick = btnSendClick
  end
end


