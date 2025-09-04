object FormJadessigner: TFormJadessigner
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JAdES Signer demo'
  ClientHeight = 344
  ClientWidth = 704
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
    Top = 8
    Width = 682
    Height = 13
    Caption = 
      'This sample illustrates the use of JAdESSigner component for cre' +
      'ating JWS/JAdES signature. Please pick the signing certificate a' +
      'nd click '#39'Sign'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lbPayload: TLabel
    Left = 8
    Top = 32
    Width = 94
    Height = 13
    Caption = 'Payload (as string):'
  end
  object Label1: TLabel
    Left = 10
    Top = 118
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 403
    Top = 113
    Width = 70
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object edOutputFile: TEdit
    Left = 71
    Top = 115
    Width = 326
    Height = 21
    TabOrder = 1
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 148
    Width = 682
    Height = 149
    Caption = 'Signing options  '
    TabOrder = 5
    object Label3: TLabel
      Left = 10
      Top = 29
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 395
      Top = 23
      Width = 70
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object Label5: TLabel
      Left = 49
      Top = 56
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label4: TLabel
      Left = 500
      Top = 29
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object Label6: TLabel
      Left = 454
      Top = 56
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object edSigningCertificate: TEdit
      Left = 105
      Top = 25
      Width = 284
      Height = 21
      TabOrder = 0
    end
    object edCertPassword: TEdit
      Left = 105
      Top = 52
      Width = 199
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object cbLevel: TComboBox
      Left = 535
      Top = 25
      Width = 138
      Height = 21
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 4
      Text = 'JAdES Baseline-B'
      Items.Strings = (
        'JSON Web Signature'
        'JAdES Baseline-B'
        'JAdES Baseline-T'
        'JAdES Baseline-LT'
        'JAdES Baseline-LTA')
    end
    object cbHashAlgorithm: TComboBox
      Left = 535
      Top = 52
      Width = 138
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'SHA256'
      Items.Strings = (
        'SHA256'
        'SHA384'
        'SHA512')
    end
    object cbRequestTimestamp: TCheckBox
      Left = 10
      Top = 94
      Width = 229
      Height = 17
      Caption = 'Request a timestamp from TSA server:'
      TabOrder = 2
    end
    object editTSPServer: TEdit
      Left = 10
      Top = 117
      Width = 379
      Height = 21
      TabOrder = 3
      Text = 'http://'
    end
  end
  object btnSign: TButton
    Left = 620
    Top = 311
    Width = 70
    Height = 25
    Caption = 'Sign'
    TabOrder = 6
    OnClick = btnSignClick
  end
  object cbCompactForm: TCheckBox
    Left = 532
    Top = 74
    Width = 97
    Height = 17
    Caption = 'Compact Form'
    TabOrder = 3
  end
  object mmPayload: TMemo
    Left = 8
    Top = 51
    Width = 465
    Height = 54
    Lines.Strings = (
      'Hello, world!')
    TabOrder = 0
  end
  object cbFlattenedSignature: TCheckBox
    Left = 532
    Top = 97
    Width = 134
    Height = 17
    Caption = 'Flattened Signature'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object cbDetached: TCheckBox
    Left = 532
    Top = 51
    Width = 97
    Height = 17
    Caption = 'Detached'
    TabOrder = 2
  end
  object dlgOpen: TOpenDialog
    Left = 160
    Top = 48
  end
  object dlgSave: TSaveDialog
    Left = 208
    Top = 48
  end
end


