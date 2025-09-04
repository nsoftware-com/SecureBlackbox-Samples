object FormSamlidpserver: TFormSamlidpserver
  Left = 95
  Top = 36
  Caption = 'SAML Identity Provider Demo'
  ClientHeight = 567
  ClientWidth = 604
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
    Top = 370
    Width = 58
    Height = 13
    Caption = 'Log Output:'
  end
  object mmLog: TMemo
    Left = 8
    Top = 408
    Width = 588
    Height = 108
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object bbStart: TButton
    Left = 155
    Top = 524
    Width = 137
    Height = 35
    Caption = 'Start Server'
    TabOrder = 1
    OnClick = bbStartClick
  end
  object bbStop: TButton
    Left = 298
    Top = 524
    Width = 137
    Height = 35
    Caption = 'Stop Server'
    Enabled = False
    TabOrder = 2
    OnClick = bbStopClick
  end
  object pcSteps: TPageControl
    Left = 0
    Top = 41
    Width = 604
    Height = 361
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Introduction'
      object mmStep1: TMemo
        Left = 16
        Top = 15
        Width = 553
        Height = 225
        Lines.Strings = (
          
            'This demo embodies a simple, standalone SAML Identity Provider s' +
            'erver. You can try it with the '
          
            'neighbouring SAML Service Provider demo, or with any compliant t' +
            'hird-party service provider.'
          ''
          
            'The easiest way to configure the demo is by clicking the "Auto C' +
            'onfiguration" button, which will '
          
            'do most of the setup for you. For more specific or custom scenar' +
            'ios, please feel free'
          'to explore/customize the settings by navigating the tabs above. '
          ''
          'General configuration steps:'
          
            '1) Provide the SSO endpoints and adjust SAML bindings as appropr' +
            'iate'
          
            '2) Provide X.509 certificates for signing, encryption, and TLS (' +
            'if used)'
          
            '3) Register SP XML metadata file for each SP you want to work wi' +
            'th'
          '4) Add login credentials for your users.'
          ''
          
            'Note that in most cases you will need to generate an IdP XML met' +
            'adata file and pass it on to the SP service. '
          'This should be done *after* you'#39've set up all the settings.'
          ''
          
            'The two SAML demos use fake sp.test.org and idp.test.org domains' +
            ' '
          'by default. Add the following lines to your '
          
            'C:\Windows\System32\drivers\etc\hosts file to let the browser re' +
            'solve them:'
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
        Top = 262
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
      Caption = 'Step 1 (General settings)'
      ImageIndex = 1
      object Label1: TLabel
        Left = 16
        Top = 13
        Width = 49
        Height = 13
        Caption = 'Base URL:'
      end
      object Label5: TLabel
        Left = 310
        Top = 13
        Width = 72
        Height = 13
        Caption = 'Metadata URL:'
      end
      object edURL: TEdit
        Left = 16
        Top = 32
        Width = 254
        Height = 21
        TabOrder = 0
        Text = 'http://127.0.0.1:64080'
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 67
        Width = 544
        Height = 102
        Caption = 'Single Sign On Service'
        TabOrder = 1
        object Label2: TLabel
          Left = 16
          Top = 21
          Width = 317
          Height = 13
          Caption = 
            'SSO URL (triggered by service providers to invoke the login form' +
            '):'
        end
        object Label6: TLabel
          Left = 352
          Top = 21
          Width = 43
          Height = 13
          Caption = 'Bindings:'
        end
        object Label8: TLabel
          Left = 16
          Top = 72
          Width = 160
          Height = 13
          Caption = 'Preffered SSO Response Binding:'
        end
        object edSSO: TEdit
          Left = 16
          Top = 40
          Width = 313
          Height = 21
          TabOrder = 0
          Text = '/idp/SingleSignOnService'
        end
        object cbSSORedirect: TCheckBox
          Left = 353
          Top = 40
          Width = 73
          Height = 17
          Caption = 'Redirect'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbSSOPOST: TCheckBox
          Left = 352
          Top = 63
          Width = 66
          Height = 17
          Caption = 'POST'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbPrefSSORespBinding: TComboBox
          Left = 192
          Top = 70
          Width = 137
          Height = 21
          ItemIndex = 0
          TabOrder = 3
          Text = 'Redirect'
          Items.Strings = (
            'Redirect'
            'POST'
            'Artifact')
        end
        object cbSSOArtifact: TCheckBox
          Left = 432
          Top = 40
          Width = 97
          Height = 17
          Caption = 'Artifact'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 174
        Width = 544
        Height = 106
        Caption = 'Single Logout Service'
        TabOrder = 2
        object Label3: TLabel
          Left = 16
          Top = 21
          Width = 322
          Height = 13
          Caption = 
            'SLS URL (triggered by service providers to discard the auth toke' +
            'n):'
        end
        object Label7: TLabel
          Left = 352
          Top = 21
          Width = 43
          Height = 13
          Caption = 'Bindings:'
        end
        object Label4: TLabel
          Left = 16
          Top = 72
          Width = 157
          Height = 13
          Caption = 'Preffered SLS Response Binding:'
        end
        object edSLS: TEdit
          Left = 16
          Top = 40
          Width = 313
          Height = 21
          TabOrder = 0
          Text = '/idp/SingleLogoutService'
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
          Left = 352
          Top = 63
          Width = 66
          Height = 17
          Caption = 'POST'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbPrefSLSRespBinding: TComboBox
          Left = 192
          Top = 70
          Width = 137
          Height = 21
          ItemIndex = 0
          TabOrder = 3
          Text = 'Redirect'
          Items.Strings = (
            'Redirect'
            'POST'
            'Artifact')
        end
        object cbSLSArtifact: TCheckBox
          Left = 431
          Top = 40
          Width = 97
          Height = 17
          Caption = 'Artifact'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
      object edMetadataURL: TEdit
        Left = 306
        Top = 32
        Width = 254
        Height = 21
        TabOrder = 3
        Text = '/idp/metadata'
      end
      object pnStep1: TPanel
        Left = 0
        Top = 299
        Width = 596
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 4
        Visible = False
        object Label15: TLabel
          Left = 92
          Top = 10
          Width = 253
          Height = 13
          Caption = 'Press the button to proceed with auto configuration:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object bbGoto2: TButton
          Left = 351
          Top = 4
          Width = 113
          Height = 25
          Caption = 'Next Step >'
          TabOrder = 0
          OnClick = bbGoto2Click
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Step 2 (Certificates)'
      ImageIndex = 2
      object Label9: TLabel
        Left = 103
        Top = 56
        Width = 199
        Height = 13
        Caption = 'Signing Certificate (private key required):'
      end
      object Label10: TLabel
        Left = 103
        Top = 104
        Width = 108
        Height = 13
        Caption = 'Encryption Certificate:'
      end
      object Label11: TLabel
        Left = 103
        Top = 200
        Width = 406
        Height = 13
        Caption = 
          'SSL/TLS Certificate (private key required; should be issued to t' +
          'he Base URL domain):'
      end
      object sbChooseSignCert: TSpeedButton
        Left = 470
        Top = 75
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseSignCertClick
      end
      object sbChooseEncCert: TSpeedButton
        Left = 470
        Top = 123
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseEncCertClick
      end
      object sbChooseServerCert: TSpeedButton
        Left = 470
        Top = 218
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseServerCertClick
      end
      object Label17: TLabel
        Left = 103
        Top = 150
        Width = 248
        Height = 13
        Caption = 'Metadata Signing Certificate (private key required):'
      end
      object sbChooseMetaSignCert: TSpeedButton
        Left = 470
        Top = 169
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseMetaSignCertClick
      end
      object Label21: TLabel
        Left = 56
        Top = 24
        Width = 479
        Height = 13
        Caption = 
          'Note: leave all the fields empty to make the Identity Provider u' +
          'se pre-generated dummy certificates'
      end
      object edSignCert: TEdit
        Left = 103
        Top = 75
        Width = 361
        Height = 21
        TabOrder = 0
      end
      object edEncCert: TEdit
        Left = 103
        Top = 123
        Width = 361
        Height = 21
        TabOrder = 1
      end
      object edServerCert: TEdit
        Left = 103
        Top = 219
        Width = 361
        Height = 21
        TabOrder = 2
      end
      object cbUseTLS: TCheckBox
        Left = 103
        Top = 246
        Width = 97
        Height = 17
        Caption = 'Use SSL/TLS'
        TabOrder = 3
      end
      object edMetaSignCert: TEdit
        Left = 103
        Top = 169
        Width = 361
        Height = 21
        TabOrder = 4
      end
      object pnStep2: TPanel
        Left = 0
        Top = 299
        Width = 596
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
        object bbGoto3: TButton
          Left = 351
          Top = 4
          Width = 113
          Height = 25
          Caption = 'Next Step >'
          TabOrder = 0
          OnClick = bbGoto3Click
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Step 3 (Metadata)'
      ImageIndex = 3
      object sbChooseSPMetadata: TSpeedButton
        Left = 475
        Top = 153
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseSPMetadataClick
      end
      object Label13: TLabel
        Left = 57
        Top = 192
        Width = 424
        Height = 13
        Caption = 
          'IDP Metadata: use the Export button to save it to a file and pas' +
          's to the service provider'
      end
      object sbChooseIDPMetadata: TSpeedButton
        Left = 424
        Top = 211
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = sbChooseIDPMetadataClick
      end
      object Label14: TLabel
        Left = 58
        Top = 239
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
      object Label12: TLabel
        Left = 58
        Top = 69
        Width = 122
        Height = 13
        Caption = 'Known Service Providers:'
      end
      object sbAddSP: TSpeedButton
        Left = 504
        Top = 153
        Width = 23
        Height = 22
        Caption = '+'
        OnClick = sbAddSPClick
      end
      object Label22: TLabel
        Left = 40
        Top = 24
        Width = 511
        Height = 26
        Caption = 
          'Provide the metadata files of the known service providers. If us' +
          'ing the two SAML demos together, use the other demo to generate ' +
          'the XML metadata file.'
        WordWrap = True
      end
      object edSPMetadata: TEdit
        Left = 58
        Top = 153
        Width = 411
        Height = 21
        TabOrder = 0
      end
      object edIDPMetadata: TEdit
        Left = 57
        Top = 211
        Width = 361
        Height = 21
        TabOrder = 1
      end
      object bbExportIDPMetadata: TBitBtn
        Left = 453
        Top = 210
        Width = 75
        Height = 25
        Caption = 'Export'
        TabOrder = 2
        OnClick = bbExportIDPMetadataClick
      end
      object lbKnownSPs: TListBox
        Left = 58
        Top = 88
        Width = 469
        Height = 59
        ItemHeight = 13
        TabOrder = 3
      end
      object pnStep3: TPanel
        Left = 0
        Top = 299
        Width = 596
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 4
        Visible = False
        object Label19: TLabel
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
          TabOrder = 0
          OnClick = bbGoto4Click
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Step 4 (Users)'
      ImageIndex = 4
      object Label23: TLabel
        Left = 42
        Top = 152
        Width = 389
        Height = 13
        Caption = 
          'Authentication form template (sent to the user'#39's browser to requ' +
          'est credentials):'
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 596
        Height = 38
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object bbAddUser: TButton
          Left = 4
          Top = 7
          Width = 92
          Height = 25
          Caption = 'Add User'
          TabOrder = 0
          OnClick = bbAddUserClick
        end
        object bbRemoveUser: TButton
          Left = 102
          Top = 7
          Width = 92
          Height = 25
          Caption = 'Remove User'
          TabOrder = 1
          OnClick = bbRemoveUserClick
        end
      end
      object lvUsers: TListView
        Left = 0
        Top = 38
        Width = 596
        Height = 99
        Columns = <
          item
            Caption = 'Login'
            Width = 130
          end
          item
            Caption = 'Password'
            Width = 130
          end
          item
            Caption = 'E-mail'
            Width = 130
          end>
        Items.ItemData = {
          05310000000100000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000
          00047500730065007200043100320033003400E0BCDC18FFFF}
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object mmLastStep: TMemo
        Left = 0
        Top = 283
        Width = 596
        Height = 50
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
          
            'Please configure SP sample first and then press <Start Server> b' +
            'utton for both samples.')
        ParentFont = False
        ReadOnly = True
        TabOrder = 2
        Visible = False
      end
      object memoAuthForm: TMemo
        Left = 42
        Top = 171
        Width = 513
        Height = 70
        Lines.Strings = (
          '<html>'
          '  <head><title>SecureBlackbox SAML Login</title></head>'
          '  <body>'
          '    <center>'
          '      <h1>Enter login credentials</h2>'
          '      <form action="%URL%" method="post">'
          '         Login:<br/><input type="text" name="login"/><br/>'
          
            '         Password:<br/><input type="password" name="password"/><' +
            'br/>'
          '        <input type="submit" value="Login"/>'
          '      </form>'
          '    </center>'
          '  </body>'
          '</html>')
        ScrollBars = ssVertical
        TabOrder = 3
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Active Sessions'
      ImageIndex = 5
      object lvSessions: TListView
        Left = 0
        Top = 0
        Width = 596
        Height = 333
        Align = alClient
        Columns = <
          item
            Caption = 'Session ID'
            Width = 160
          end
          item
            Caption = 'Status'
            Width = 400
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 604
    Height = 41
    Align = alTop
    TabOrder = 4
    object Label20: TLabel
      Left = 8
      Top = 14
      Width = 202
      Height = 13
      Caption = 'This demo is a basic SAML Identity server.'
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
  object SaveDlg: TSaveDialog
    Left = 392
    Top = 408
  end
end


