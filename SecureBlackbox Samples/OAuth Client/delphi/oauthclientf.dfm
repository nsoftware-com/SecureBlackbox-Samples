object FormOAuthClient: TFormOAuthClient
  Left = 276
  Top = 186
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OAuth Client Sample'
  ClientHeight = 689
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    529
    689)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 513
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample shows how to use OAuthClient to authorize the users ' +
      'with OAuth 2.0 servers.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object grpParams: TGroupBox
    Left = 8
    Top = 91
    Width = 513
    Height = 390
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Parameters '
    TabOrder = 1
    DesignSize = (
      513
      390)
    object lblAuthUrl: TLabel
      Left = 15
      Top = 24
      Width = 135
      Height = 13
      Caption = 'Authorization Endpoint URL:'
    end
    object lblTokenUrl: TLabel
      Left = 15
      Top = 67
      Width = 100
      Height = 13
      Caption = 'Token Endpoint URL:'
    end
    object lblClientId: TLabel
      Left = 15
      Top = 110
      Width = 45
      Height = 13
      Caption = 'Client ID:'
    end
    object lblClientSecret: TLabel
      Left = 15
      Top = 152
      Width = 65
      Height = 13
      Caption = 'Client Secret:'
    end
    object lblScope: TLabel
      Left = 15
      Top = 194
      Width = 158
      Height = 13
      Caption = 'Scope (resource server specific):'
    end
    object lblRedirectUrl: TLabel
      Left = 15
      Top = 236
      Width = 66
      Height = 13
      Caption = 'Redirect URL:'
    end
    object lblRefreshToken: TLabel
      Left = 15
      Top = 291
      Width = 74
      Height = 13
      Caption = 'Refresh Token:'
    end
    object lblUsername: TLabel
      Left = 15
      Top = 333
      Width = 52
      Height = 13
      Caption = 'Username:'
    end
    object lblPassword: TLabel
      Left = 255
      Top = 333
      Width = 50
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Password:'
    end
    object edtAuthUrl: TEdit
      Left = 15
      Top = 40
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'https://accounts.google.com/o/oauth2/auth'
    end
    object edtTokenUrl: TEdit
      Left = 15
      Top = 83
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'https://accounts.google.com/o/oauth2/token'
    end
    object edtClientId: TEdit
      Left = 15
      Top = 125
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 
        '[client id you'#39've got when registered the app on the auth server' +
        ']'
    end
    object edtClientSecret: TEdit
      Left = 15
      Top = 167
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = 
        '[client secret you'#39've got when registered the app on the auth se' +
        'rver]'
    end
    object edtScope: TEdit
      Left = 15
      Top = 209
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      Text = 
        'https://www.googleapis.com/auth/userinfo.email https://www.googl' +
        'eapis.com/auth/userinfo.profile'
    end
    object edtRedirectUrl: TEdit
      Left = 15
      Top = 252
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      Text = '[redirect url you'#39've registered on the auth server]'
    end
    object edtRefreshToken: TEdit
      Left = 15
      Top = 306
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
    end
    object edtUsername: TEdit
      Left = 15
      Top = 348
      Width = 234
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
    end
    object edtPassword: TEdit
      Left = 255
      Top = 348
      Width = 244
      Height = 21
      Anchors = [akTop, akRight]
      PasswordChar = '*'
      TabOrder = 8
    end
  end
  object grpGrantType: TRadioGroup
    Left = 8
    Top = 27
    Width = 513
    Height = 58
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Grant Type '
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'Authorization Code'
      'Implicit'
      'Password Credentials'
      'Client Credentials')
    TabOrder = 0
    OnClick = grpGrantTypeClick
  end
  object grpResults: TGroupBox
    Left = 8
    Top = 518
    Width = 513
    Height = 163
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Results '
    TabOrder = 3
    DesignSize = (
      513
      163)
    object lblAccessToken: TLabel
      Left = 15
      Top = 24
      Width = 69
      Height = 13
      Caption = 'Access Token:'
    end
    object lblExpiration: TLabel
      Left = 15
      Top = 66
      Width = 124
      Height = 13
      Caption = 'Expiration Date and Time:'
    end
    object lblTokenType: TLabel
      Left = 255
      Top = 66
      Width = 60
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Token Type:'
    end
    object lblNewRefreshToken: TLabel
      Left = 15
      Top = 108
      Width = 98
      Height = 13
      Caption = 'New Refresh Token:'
    end
    object edtAccessToken: TEdit
      Left = 15
      Top = 39
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 0
    end
    object edtExpiration: TEdit
      Left = 15
      Top = 81
      Width = 234
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 1
    end
    object edtTokenType: TEdit
      Left = 255
      Top = 81
      Width = 244
      Height = 21
      Anchors = [akTop, akRight]
      ReadOnly = True
      TabOrder = 2
    end
    object edtNewRefreshToken: TEdit
      Left = 15
      Top = 124
      Width = 484
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 3
    end
  end
  object btnAuthorize: TButton
    Left = 222
    Top = 487
    Width = 75
    Height = 25
    Caption = 'Authorize'
    TabOrder = 2
    OnClick = btnAuthorizeClick
  end
end


