object FormMailReader: TFormMailReader
  Left = 574
  Top = 325
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Mail Reader'
  ClientHeight = 655
  ClientWidth = 948
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
    948
    655)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 932
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample shows how to parse an e-mail message, including sign' +
      'ed  and/or encrypted messages.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object grpLoad: TGroupBox
    Left = 8
    Top = 27
    Width = 431
    Height = 86
    Caption = ' Load and Parse '
    TabOrder = 0
    object lblFilename: TLabel
      Left = 12
      Top = 24
      Width = 46
      Height = 13
      Caption = 'Filename:'
    end
    object edtFilename: TEdit
      Left = 64
      Top = 21
      Width = 319
      Height = 21
      TabOrder = 0
    end
    object btnFilename: TButton
      Left = 389
      Top = 21
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnFilenameClick
    end
    object btnLoad: TButton
      Left = 134
      Top = 51
      Width = 225
      Height = 23
      Caption = 'Load and Parse'
      TabOrder = 2
      OnClick = btnLoadClick
    end
  end
  object grpText: TGroupBox
    Left = 445
    Top = 120
    Width = 493
    Height = 380
    Caption = ' Text '
    TabOrder = 5
    object lblPlainText: TLabel
      Left = 12
      Top = 21
      Width = 49
      Height = 13
      Caption = 'Plain text:'
    end
    object lblHtmlText: TLabel
      Left = 12
      Top = 197
      Width = 53
      Height = 13
      Caption = 'HTML text:'
    end
    object memPlainText: TMemo
      Left = 12
      Top = 40
      Width = 469
      Height = 148
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object memHtmlText: TMemo
      Left = 12
      Top = 216
      Width = 469
      Height = 148
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object grpAttachments: TGroupBox
    Left = 447
    Top = 506
    Width = 493
    Height = 141
    Caption = ' Attachments '
    TabOrder = 6
    object lvwAttachments: TListView
      Left = 12
      Top = 23
      Width = 469
      Height = 78
      Columns = <
        item
          Caption = 'Filename/ID'
          Width = 235
        end
        item
          Caption = 'Content type'
          Width = 140
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
      TabOrder = 0
      ViewStyle = vsReport
    end
    object btnAttach: TButton
      Left = 406
      Top = 107
      Width = 75
      Height = 23
      Caption = 'Save As...'
      TabOrder = 1
      OnClick = btnAttachClick
    end
  end
  object grpOriginators: TGroupBox
    Left = 8
    Top = 119
    Width = 431
    Height = 83
    Caption = ' Originator(s) '
    TabOrder = 1
    object lblFrom: TLabel
      Left = 22
      Top = 51
      Width = 28
      Height = 13
      Caption = 'From:'
    end
    object lblSender: TLabel
      Left = 12
      Top = 24
      Width = 38
      Height = 13
      Caption = 'Sender:'
    end
    object edtSender: TEdit
      Left = 56
      Top = 21
      Width = 363
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object edtFrom: TEdit
      Left = 56
      Top = 48
      Width = 363
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
  end
  object grpRecipients: TGroupBox
    Left = 8
    Top = 208
    Width = 431
    Height = 267
    Caption = ' Recipients '
    TabOrder = 2
    object lblTo: TLabel
      Left = 12
      Top = 21
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object lblCc: TLabel
      Left = 12
      Top = 141
      Width = 16
      Height = 13
      Caption = 'Cc:'
    end
    object lvwTo: TListView
      Left = 12
      Top = 40
      Width = 407
      Height = 95
      Columns = <
        item
          Caption = 'Name'
          Width = 193
        end
        item
          Caption = 'Address'
          Width = 193
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object lvwCc: TListView
      Left = 12
      Top = 160
      Width = 407
      Height = 95
      Columns = <
        item
          Caption = 'Name'
          Width = 193
        end
        item
          Caption = 'Address'
          Width = 193
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
    end
  end
  object grpInfo: TGroupBox
    Left = 445
    Top = 27
    Width = 493
    Height = 87
    Caption = ' Info '
    TabOrder = 4
    object lblSubject: TLabel
      Left = 12
      Top = 27
      Width = 40
      Height = 13
      Caption = 'Subject:'
    end
    object lblPriority: TLabel
      Left = 14
      Top = 55
      Width = 38
      Height = 13
      Caption = 'Priority:'
    end
    object txtPriority: TLabel
      Left = 58
      Top = 55
      Width = 51
      Height = 13
      AutoSize = False
      Caption = 'Normal'
    end
    object edtSubject: TEdit
      Left = 58
      Top = 24
      Width = 423
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object cbxDeliveryReceipt: TCheckBox
      Left = 126
      Top = 54
      Width = 151
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Delivery receipt requested:'
      Enabled = False
      TabOrder = 1
    end
    object cbxReadReceipt: TCheckBox
      Left = 319
      Top = 54
      Width = 135
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Read receipt requested:'
      Enabled = False
      TabOrder = 2
    end
  end
  object grpSecurity: TGroupBox
    Left = 8
    Top = 481
    Width = 431
    Height = 165
    Caption = ' Security Info '
    TabOrder = 3
    object lblDecryptionCertificate: TLabel
      Left = 12
      Top = 19
      Width = 107
      Height = 13
      Caption = 'Decryption certificate:'
    end
    object lblEncryptionAlgorithm: TLabel
      Left = 12
      Top = 65
      Width = 102
      Height = 13
      Caption = 'Encryption algorithm:'
    end
    object lblSigningCertificate: TLabel
      Left = 12
      Top = 93
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object lblSignatureValidation: TLabel
      Left = 12
      Top = 139
      Width = 99
      Height = 13
      Caption = 'Signature validation:'
    end
    object lblHashAlgorithm: TLabel
      Left = 243
      Top = 139
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object edtDecryptionCertificate: TEdit
      Left = 12
      Top = 38
      Width = 407
      Height = 21
      ReadOnly = True
      TabOrder = 0
      Text = '[message not encrypted]'
    end
    object txtEncryptionAlgorithm: TStaticText
      Left = 120
      Top = 65
      Width = 299
      Height = 17
      AutoSize = False
      TabOrder = 1
    end
    object edtSigningCertificate: TEdit
      Left = 12
      Top = 112
      Width = 407
      Height = 21
      ReadOnly = True
      TabOrder = 2
      Text = '[message not signed]'
    end
    object txtSignatureValidation: TStaticText
      Left = 120
      Top = 139
      Width = 103
      Height = 17
      AutoSize = False
      TabOrder = 3
    end
    object txtHashAlgorithm: TStaticText
      Left = 324
      Top = 139
      Width = 95
      Height = 17
      AutoSize = False
      TabOrder = 4
    end
  end
  object dlgLoadMessage: TOpenDialog
    Filter = 'E-mail Messages (*.eml, *.msg)|*.eml;*.msg|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Load a Message'
    Left = 32
    Top = 69
  end
  object dlgSaveAttachment: TSaveDialog
    Filter = 'All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save an Attachment'
    Left = 604
    Top = 587
  end
end


