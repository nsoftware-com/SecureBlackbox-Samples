object FormMailWriter: TFormMailWriter
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Mail Writer Demo'
  ClientHeight = 633
  ClientWidth = 961
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
    961
    633)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 945
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample shows how to compose an e-mail message. Also, it'#39's p' +
      'ossible to sign and/or encrypt the message with certificates.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object grpOriginators: TGroupBox
    Left = 8
    Top = 27
    Width = 446
    Height = 100
    Caption = ' Originator(s) '
    TabOrder = 0
    object lblFrom: TLabel
      Left = 22
      Top = 27
      Width = 28
      Height = 13
      Caption = 'From:'
    end
    object lblSender: TLabel
      Left = 12
      Top = 54
      Width = 38
      Height = 13
      Caption = 'Sender:'
    end
    object lblSenderNotice: TLabel
      Left = 56
      Top = 73
      Width = 379
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 
        '(required only if there are several addresses specified in the "' +
        'From" field)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
    end
    object edtFrom: TEdit
      Left = 56
      Top = 24
      Width = 343
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object edtSender: TEdit
      Left = 56
      Top = 51
      Width = 379
      Height = 21
      TabOrder = 2
    end
    object btnFrom: TButton
      Left = 405
      Top = 24
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnFromClick
    end
  end
  object grpAddressees: TGroupBox
    Left = 8
    Top = 133
    Width = 446
    Height = 114
    Caption = ' Addressees '
    TabOrder = 1
    object lblTo: TLabel
      Left = 16
      Top = 27
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object lblCc: TLabel
      Left = 16
      Top = 54
      Width = 16
      Height = 13
      Caption = 'Cc:'
    end
    object lblBcc: TLabel
      Left = 12
      Top = 81
      Width = 20
      Height = 13
      Caption = 'Bcc:'
    end
    object edtTo: TEdit
      Left = 38
      Top = 24
      Width = 361
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object btnTo: TButton
      Left = 405
      Top = 24
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnToClick
    end
    object edtCc: TEdit
      Left = 38
      Top = 51
      Width = 361
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
    object btnCc: TButton
      Left = 405
      Top = 51
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = btnCcClick
    end
    object edtBcc: TEdit
      Left = 38
      Top = 78
      Width = 361
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
    end
    object btnBcc: TButton
      Left = 405
      Top = 78
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = btnBccClick
    end
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 253
    Width = 446
    Height = 87
    Caption = ' Options '
    TabOrder = 2
    object lblSubject: TLabel
      Left = 12
      Top = 27
      Width = 40
      Height = 13
      Caption = 'Subject:'
    end
    object lblPriority: TLabel
      Left = 14
      Top = 54
      Width = 38
      Height = 13
      Caption = 'Priority:'
    end
    object edtSubject: TEdit
      Left = 58
      Top = 24
      Width = 377
      Height = 21
      TabOrder = 0
    end
    object cmbPriority: TComboBox
      Left = 58
      Top = 51
      Width = 69
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 1
      Text = 'Normal'
      Items.Strings = (
        'Lowest'
        'Low'
        'Normal'
        'High'
        'Highest')
    end
    object cbxDeliveryReceipt: TCheckBox
      Left = 144
      Top = 53
      Width = 142
      Height = 17
      Caption = 'Request delivery receipt'
      TabOrder = 2
    end
    object cbxReadReceipt: TCheckBox
      Left = 301
      Top = 53
      Width = 129
      Height = 17
      Caption = 'Request read receipt'
      TabOrder = 3
    end
  end
  object grpSecurity: TGroupBox
    Left = 8
    Top = 346
    Width = 446
    Height = 279
    Caption = ' Security '
    TabOrder = 3
    object lblSigningCertificate: TLabel
      Left = 12
      Top = 24
      Width = 284
      Height = 13
      Caption = 'Signing certificate (should have an associated private key):'
    end
    object lblHashAlgorithm: TLabel
      Left = 12
      Top = 73
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lblSignatureFormat: TLabel
      Left = 231
      Top = 73
      Width = 85
      Height = 13
      Caption = 'Signature format:'
    end
    object lblEncryptionCertificates: TLabel
      Left = 12
      Top = 120
      Width = 411
      Height = 13
      Caption = 
        'Encryption certificates (one certificate for each addressee, pri' +
        'vate keys not needed):'
    end
    object lblEncryptionAlgorithm: TLabel
      Left = 12
      Top = 227
      Width = 102
      Height = 13
      Caption = 'Encryption algorithm:'
    end
    object lblHashAlgorithmNotice: TLabel
      Left = 93
      Top = 92
      Width = 101
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '(not all listed)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblEncryptionCertificateNotice: TLabel
      Left = 120
      Top = 246
      Width = 97
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '(not all listed)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lvwEncryptionCertificates: TListView
      Left = 12
      Top = 139
      Width = 423
      Height = 78
      Columns = <
        item
          Caption = 'Subject'
          Width = 200
        end
        item
          Caption = 'Issuer'
          Width = 200
        end>
      ColumnClick = False
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 5
      ViewStyle = vsReport
    end
    object cmbHashAlgorithm: TComboBox
      Left = 93
      Top = 70
      Width = 101
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 3
      Text = 'SHA-256'
      Items.Strings = (
        'MD5 (legacy)'
        'SHA-1 (legacy)'
        'SHA-224'
        'SHA-256'
        'SHA-384'
        'SHA-512'
        'SHA3-224'
        'SHA3-256'
        'SHA3-384'
        'SHA3-512')
    end
    object cmbSignatureFormat: TComboBox
      Left = 322
      Top = 70
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'multipart/signed'
      Items.Strings = (
        'multipart/signed'
        'signed-data')
    end
    object btnLoadSigningCertificate: TButton
      Left = 369
      Top = 43
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnLoadSigningCertificateClick
    end
    object btnLoadEncryptionCertificate: TButton
      Left = 279
      Top = 223
      Width = 75
      Height = 23
      Caption = 'Load...'
      TabOrder = 6
      OnClick = btnLoadEncryptionCertificateClick
    end
    object btnDeleteEncryptionCertificate: TButton
      Left = 360
      Top = 223
      Width = 75
      Height = 23
      Caption = 'Delete'
      TabOrder = 7
      OnClick = btnDeleteEncryptionCertificateClick
    end
    object cmbEncryptionAlgorithm: TComboBox
      Left = 120
      Top = 224
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 8
      Text = 'AES 128'
      Items.Strings = (
        'DES (legacy)'
        'Tripple DES'
        'AES 128'
        'AES 192'
        'AES 256'
        'Blowfish'
        'TwoFish'
        'Camellia'
        'Serpent')
    end
    object btnClearSigningCertificate: TButton
      Left = 405
      Top = 43
      Width = 30
      Height = 21
      Caption = 'X'
      TabOrder = 2
      OnClick = btnClearSigningCertificateClick
    end
    object edtSigningCertificate: TEdit
      Left = 12
      Top = 43
      Width = 351
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
  end
  object grpText: TGroupBox
    Left = 460
    Top = 27
    Width = 493
    Height = 359
    Caption = ' Text '
    TabOrder = 4
    object lblPlainText: TLabel
      Left = 12
      Top = 21
      Width = 49
      Height = 13
      Caption = 'Plain text:'
    end
    object lblHtmlText: TLabel
      Left = 12
      Top = 165
      Width = 53
      Height = 13
      Caption = 'HTML text:'
    end
    object lblEmbeddedImagesNotice: TLabel
      Left = 12
      Top = 319
      Width = 388
      Height = 26
      Alignment = taCenter
      AutoSize = False
      Caption = 
        '(if the HTML text contains references to images that need to be ' +
        'embedded into the message, use the button to the right to add th' +
        'em)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object memPlainText: TMemo
      Left = 12
      Top = 40
      Width = 469
      Height = 117
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object memHtmlText: TMemo
      Left = 12
      Top = 184
      Width = 469
      Height = 129
      ScrollBars = ssBoth
      TabOrder = 1
    end
    object btnEmbed: TButton
      Left = 406
      Top = 319
      Width = 75
      Height = 23
      Caption = 'Embed...'
      TabOrder = 2
      OnClick = btnEmbedClick
    end
  end
  object grpAttachments: TGroupBox
    Left = 460
    Top = 392
    Width = 493
    Height = 141
    Caption = ' Attachments '
    TabOrder = 5
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
      Left = 325
      Top = 107
      Width = 75
      Height = 23
      Caption = 'Attach...'
      TabOrder = 1
      OnClick = btnAttachClick
    end
    object btnDeleteAttachment: TButton
      Left = 406
      Top = 107
      Width = 75
      Height = 23
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteAttachmentClick
    end
  end
  object grpSave: TGroupBox
    Left = 460
    Top = 539
    Width = 493
    Height = 86
    Caption = ' Assemble and Save '
    TabOrder = 6
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
      Width = 381
      Height = 21
      TabOrder = 0
    end
    object btnFilename: TButton
      Left = 451
      Top = 21
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnFilenameClick
    end
    object btnSave: TButton
      Left = 134
      Top = 51
      Width = 225
      Height = 23
      Caption = 'Assemble and Save'
      TabOrder = 2
      OnClick = btnSaveClick
    end
  end
  object dlgAttachFile: TOpenDialog
    Filter = 'All Files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Attach a File'
    Left = 706
    Top = 482
  end
  object dlgSaveMessage: TSaveDialog
    Filter = 'E-mail Messages (*.eml, *.msg)|*.eml;*.msg|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save a Message'
    Left = 856
    Top = 578
  end
  object dlgEmbedImage: TOpenDialog
    Filter = 'Image Files|*.jpg;*.png;*.gif|All Files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Embed an Image'
    Left = 787
    Top = 269
  end
  object dlgSigningCertificate: TOpenDialog
    Filter = 'Certificates (*.pfx, *.p12)|*.pfx;*.p12|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Load a Signing Certificate'
    Left = 363
    Top = 342
  end
  object dlgEncryptionCertificate: TOpenDialog
    Filter = 
      'Certificates (*.cer, *.pem, *.pfx, *.p12)|*.cer;*.pem;*.pfx;*.p1' +
      '2|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Load an Encryption Certificate'
    Left = 246
    Top = 507
  end
end


