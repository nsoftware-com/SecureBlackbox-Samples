object FormSamlspserver: TFormSamlspserver
  Left = 0
  Top = 0
  Caption = 'SAML Service Provider Demo'
  ClientHeight = 578
  ClientWidth = 592
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
  object Label16: TLabel
    Left = 8
    Top = 373
    Width = 58
    Height = 13
    Caption = 'Log Output:'
  end
  object pcSteps: TPageControl
    Left = 0
    Top = 41
    Width = 592
    Height = 361
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Introduction'
      object mmStep1: TMemo
        Left = 3
        Top = 15
        Width = 570
        Height = 242
        Lines.Strings = (
          
            'The demo represents a simple, standalone SAML Service Provider s' +
            'erver. You can use it with the SAML IdP'
          
            'counterpart that you will find alongside it, or with a compliant' +
            ' third-party SAML Identity Provider.'
          ''
          
            'The easiest way to configure the demo is by clicking the "Auto C' +
            'onfiguration" button, which will do most'
          
            'of the setup for you. For more specific or custom scenarios, ple' +
            'ase feel free to explore/customize'
          'the settings by navigating the tabs above.'
          ''
          'Here'#39's the general configuration steps that you need to take:'
          
            '1) Configure the server endpoints and enable required SAML bindi' +
            'ngs'
          
            '2) Provide X.509 certificates for signing, encryption, and (opti' +
            'onally) TLS'
          '3) Register the IdP service XML metadata file'
          
            '4) Set the web site directory containing resources managed by th' +
            'is Service Provider'
          ''
          
            'Note that in most cases you will need to generate an SP XML meta' +
            'data file and pass it on to the IdP service. '
          'This should be done *after* you'#39've set up all the settings.'
          ''
          
            'The two SAML demos use fake sp.test.org and idp.test.org domains' +
            ' by default. '
          
            'Add the following lines to your C:\Windows\System32\drivers\etc\' +
            'hosts file to let the browser resolve them:'
          ''
          '127.0.0.1 sp.test.org'
          '127.0.0.1 idp.test.org'
          ''
          
            'If registering the domains in the above way is not possible, ple' +
            'ase use the IP addresses instead.'
          ''
          
            'If running the IdP and SP samples on the same system, please mak' +
            'e sure '
          
            'to assign them to different listening ports. The default configu' +
            'ration takes care of that.'
          ''
          'When ready, click "Start Server" to activate the server.')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object bbAutoConfig: TButton
        Left = 193
        Top = 276
        Width = 200
        Height = 41
        Caption = 'Auto Configuration'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bbAutoConfigClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Step 1 (General Settings)'
      ImageIndex = 1
      object Label1: TLabel
        Left = 16
        Top = 13
        Width = 49
        Height = 13
        Caption = 'Base URL:'
      end
      object Label4: TLabel
        Left = 16
        Top = 59
        Width = 59
        Height = 13
        Caption = 'Logout URL:'
      end
      object Label5: TLabel
        Left = 216
        Top = 59
        Width = 72
        Height = 13
        Caption = 'Metadata URL:'
      end
      object edURL: TEdit
        Left = 16
        Top = 32
        Width = 553
        Height = 21
        TabOrder = 0
        Text = 'http://127.0.0.1:65080'
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 115
        Width = 553
        Height = 78
        Caption = 'Assertion Consumer Service'
        TabOrder = 1
        object Label2: TLabel
          Left = 16
          Top = 21
          Width = 46
          Height = 13
          Caption = 'ACS URL:'
        end
        object Label6: TLabel
          Left = 352
          Top = 21
          Width = 43
          Height = 13
          Caption = 'Bindings:'
        end
        object edACS: TEdit
          Left = 16
          Top = 40
          Width = 313
          Height = 21
          TabOrder = 0
          Text = '/sp/AssertionConsumerService'
        end
        object cbACSRedirect: TCheckBox
          Left = 352
          Top = 40
          Width = 73
          Height = 17
          Caption = 'Redirect'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbACSPOST: TCheckBox
          Left = 420
          Top = 40
          Width = 66
          Height = 17
          Caption = 'POST'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbACSArtifact: TCheckBox
          Left = 480
          Top = 40
          Width = 70
          Height = 17
          Caption = 'Artifact'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 199
        Width = 553
        Height = 78
        Caption = 'Single Logout Service'
        TabOrder = 2
        object Label3: TLabel
          Left = 16
          Top = 21
          Width = 43
          Height = 13
          Caption = 'SLS URL:'
        end
        object Label7: TLabel
          Left = 352
          Top = 21
          Width = 43
          Height = 13
          Caption = 'Bindings:'
        end
        object edSLS: TEdit
          Left = 16
          Top = 40
          Width = 313
          Height = 21
          TabOrder = 0
          Text = '/sp/SingleLogoutService'
        end
        object cbSLSRedirect: TCheckBox
          Left = 352
          Top = 40
          Width = 73
          Height = 17
          Caption = 'Redirect'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbSLSPOST: TCheckBox
          Left = 420
          Top = 40
          Width = 93
          Height = 17
          Caption = 'POST'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbSLSArtifact: TCheckBox
          Left = 480
          Top = 40
          Width = 70
          Height = 17
          Caption = 'Artifact'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object edLogoutPage: TEdit
        Left = 16
        Top = 78
        Width = 170
        Height = 21
        TabOrder = 3
        Text = '/sp/logout'
      end
      object edMetadataURL: TEdit
        Left = 216
        Top = 78
        Width = 170
        Height = 21
        TabOrder = 4
        Text = '/sp/metadata'
      end
      object pnStep1: TPanel
        Left = 0
        Top = 299
        Width = 584
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 5
        Visible = False
        object Label18: TLabel
          Left = 92
          Top = 10
          Width = 253
          Height = 13
          Caption = 'Press the button to proceed with auto configuration:'
        end
        object bbGoto2: TButton
          Left = 351
          Top = 4
          Width = 113
          Height = 25
          Caption = 'Next Step >'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = bbGoto2Click
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Step 2 (Certificates)'
      ImageIndex = 2
      object Label9: TLabel
        Left = 96
        Top = 40
        Width = 91
        Height = 13
        Caption = 'Signing Certificate:'
      end
      object Label10: TLabel
        Left = 96
        Top = 88
        Width = 108
        Height = 13
        Caption = 'Encryption Certificate:'
      end
      object Label11: TLabel
        Left = 96
        Top = 184
        Width = 130
        Height = 13
        Caption = 'Server SSL/TLS Certificate:'
      end
      object sbChooseSignCert: TSpeedButton
        Left = 463
        Top = 59
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseSignCertClick
      end
      object sbChooseEncCert: TSpeedButton
        Left = 463
        Top = 107
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseEncCertClick
      end
      object sbChooseServerCert: TSpeedButton
        Left = 463
        Top = 202
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseServerCertClick
      end
      object Label17: TLabel
        Left = 96
        Top = 134
        Width = 140
        Height = 13
        Caption = 'Metadata Signing Certificate:'
      end
      object sbChooseMetaSignCert: TSpeedButton
        Left = 463
        Top = 153
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseMetaSignCertClick
      end
      object edSignCert: TEdit
        Left = 96
        Top = 59
        Width = 361
        Height = 21
        TabOrder = 0
      end
      object edEncCert: TEdit
        Left = 96
        Top = 107
        Width = 361
        Height = 21
        TabOrder = 1
      end
      object edServerCert: TEdit
        Left = 96
        Top = 203
        Width = 361
        Height = 21
        TabOrder = 2
      end
      object cbUseTLS: TCheckBox
        Left = 96
        Top = 230
        Width = 97
        Height = 17
        Caption = 'Use SSL/TLS'
        TabOrder = 3
      end
      object edMetaSignCert: TEdit
        Left = 96
        Top = 153
        Width = 361
        Height = 21
        TabOrder = 4
      end
      object pnStep2: TPanel
        Left = 0
        Top = 299
        Width = 584
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 5
        Visible = False
        object Label19: TLabel
          Left = 92
          Top = 10
          Width = 253
          Height = 13
          Caption = 'Press the button to proceed with auto configuration:'
        end
        object bbGoto3: TButton
          Left = 351
          Top = 4
          Width = 113
          Height = 25
          Caption = 'Next Step >'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = bbGoto3Click
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 271
        Width = 584
        Height = 28
        Align = alBottom
        Alignment = taCenter
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Lines.Strings = (
          'Leave inputs empty to generate test certificates automaticaly.')
        ParentFont = False
        ReadOnly = True
        TabOrder = 6
        Visible = False
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Step 3 (Metadata)'
      ImageIndex = 3
      object Label12: TLabel
        Left = 56
        Top = 80
        Width = 70
        Height = 13
        Caption = 'IDP Metadata:'
      end
      object sbChooseIDPMetadata: TSpeedButton
        Left = 423
        Top = 99
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseIDPMetadataClick
      end
      object Label13: TLabel
        Left = 56
        Top = 136
        Width = 65
        Height = 13
        Caption = 'SP Metadata:'
      end
      object sbChooseSPMetadata: TSpeedButton
        Left = 423
        Top = 155
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseSPMetadataClick
      end
      object Label14: TLabel
        Left = 57
        Top = 183
        Width = 389
        Height = 13
        Caption = 
          'Metadata can be also downloaded using metadata URL set on "Base ' +
          'Options" tab'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object Label22: TLabel
        Left = 56
        Top = 24
        Width = 387
        Height = 39
        Caption = 
          'Provide the location of the IdP XML metadata file. Make sure the' +
          ' resources path is set (Step 4), and then generate and export th' +
          'e SP metadata file using the Export button below. Pass the creat' +
          'ed metadata file to the IdP.'
        WordWrap = True
      end
      object edIDPMetadata: TEdit
        Left = 56
        Top = 99
        Width = 361
        Height = 21
        TabOrder = 0
      end
      object edSPMetadata: TEdit
        Left = 56
        Top = 155
        Width = 361
        Height = 21
        TabOrder = 1
      end
      object bbExportSPMetadata: TBitBtn
        Left = 452
        Top = 152
        Width = 75
        Height = 25
        Caption = 'Export'
        TabOrder = 2
        OnClick = bbExportSPMetadataClick
      end
      object pnStep3: TPanel
        Left = 0
        Top = 299
        Width = 584
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 3
        Visible = False
        object Label20: TLabel
          Left = 92
          Top = 10
          Width = 253
          Height = 13
          Caption = 'Press the button to proceed with auto configuration:'
        end
        object bbGoto4: TButton
          Left = 351
          Top = 4
          Width = 113
          Height = 25
          Caption = 'Next Step >'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = bbGoto4Click
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Step 4 (Resources)'
      ImageIndex = 4
      object Label15: TLabel
        Left = 88
        Top = 112
        Width = 348
        Height = 13
        Caption = 
          'A folder containing the secure part of the web site protected wi' +
          'th SAML:'
      end
      object sbChooseProtectedRes: TSpeedButton
        Left = 455
        Top = 131
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseProtectedResClick
      end
      object edProtectedResources: TEdit
        Left = 88
        Top = 131
        Width = 361
        Height = 21
        TabOrder = 0
      end
      object mmLastStep: TMemo
        Left = 0
        Top = 253
        Width = 584
        Height = 80
        Align = alBottom
        Alignment = taCenter
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Lines.Strings = (
          ''
          'Auto configuration finished!'
          
            'Please configure IdP sample first and then press <Start Server> ' +
            'button in both samples.'
          ''
          'Once started, navigate to %URL% in your web browser.')
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
        Visible = False
      end
    end
  end
  object mmLog: TMemo
    Left = 8
    Top = 408
    Width = 576
    Height = 113
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object bbStart: TButton
    Left = 155
    Top = 527
    Width = 137
    Height = 35
    Caption = 'Start Server'
    TabOrder = 2
    OnClick = bbStartClick
  end
  object bbStop: TButton
    Left = 298
    Top = 527
    Width = 137
    Height = 35
    Caption = 'Stop Server'
    Enabled = False
    TabOrder = 3
    OnClick = bbStopClick
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 592
    Height = 41
    Align = alTop
    TabOrder = 4
    object Label21: TLabel
      Left = 8
      Top = 14
      Width = 375
      Height = 13
      Caption = 
        'This demo is a basic SAML Service Provider server (e.g. a protec' +
        'ted web site).'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object OpenDlg: TOpenDialog
    Left = 500
    Top = 440
  end
end


