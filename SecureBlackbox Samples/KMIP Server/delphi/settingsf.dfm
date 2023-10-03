object Formsettings: TFormsettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 536
  ClientWidth = 444
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btOk: TButton
    Left = 270
    Top = 503
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
    OnClick = btOkClick
  end
  object GroupBox1: TGroupBox
    Left = 5
    Top = 5
    Width = 430
    Height = 480
    Caption = 'Server settings  '
    TabOrder = 0
    object Label2: TLabel
      Left = 15
      Top = 130
      Width = 70
      Height = 13
      Caption = 'Listen on port:'
    end
    object Label1: TLabel
      Left = 15
      Top = 25
      Width = 59
      Height = 13
      Caption = 'Storage file:'
    end
    object sbBrowseStorage: TSpeedButton
      Left = 347
      Top = 39
      Width = 70
      Height = 22
      Caption = 'Browse...'
      OnClick = sbBrowseStorageClick
    end
    object gbUsers: TGroupBox
      Left = 15
      Top = 353
      Width = 410
      Height = 120
      Caption = 'Authorized users. If none, everyone will be allowed in this demo'
      TabOrder = 9
      DesignSize = (
        410
        120)
      object btnAdd: TBitBtn
        Left = 332
        Top = 20
        Width = 70
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Add'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          7972FF7972FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7972FF7972FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          7972FF7972FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7972FF7972FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7972FF7972FF7972FF7972FF
          7972FF7972FF7972FF7972FF7972FF7972FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF7972FF7972FF7972FF7972FF7972FF7972FF7972FF7972FF7972FF79
          72FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          7972FF7972FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7972FF7972FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          7972FF7972FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7972FF7972FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        TabOrder = 0
        OnClick = btnAddClick
      end
      object btnRemove: TBitBtn
        Left = 332
        Top = 55
        Width = 70
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Remove'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        TabOrder = 1
        OnClick = btnRemoveClick
      end
      object lbUsers: TListBox
        Left = 10
        Top = 20
        Width = 315
        Height = 90
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object edListenPort: TEdit
      Left = 99
      Top = 126
      Width = 65
      Height = 21
      TabOrder = 2
      Text = '5696'
    end
    object cbUseSSL: TCheckBox
      Left = 195
      Top = 130
      Width = 97
      Height = 17
      Caption = 'Use SSL/TLS'
      TabOrder = 3
    end
    object cbUseCompression: TCheckBox
      Left = 15
      Top = 288
      Width = 121
      Height = 17
      Caption = 'Use compression'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object cbUseChunking: TCheckBox
      Left = 179
      Top = 288
      Width = 169
      Height = 17
      Caption = 'Use chunked encoding'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object cbBasicAuth: TCheckBox
      Left = 15
      Top = 320
      Width = 129
      Height = 17
      Caption = 'Basic authentication'
      TabOrder = 7
    end
    object cbDigestAuth: TCheckBox
      Left = 179
      Top = 320
      Width = 161
      Height = 17
      Caption = 'Digest authentication'
      TabOrder = 8
    end
    object edStorageFile: TEdit
      Left = 15
      Top = 40
      Width = 326
      Height = 21
      TabOrder = 0
    end
    object rgEncoderType: TRadioGroup
      Left = 15
      Top = 70
      Width = 233
      Height = 40
      Caption = 'Encoder type'
      Columns = 3
      Items.Strings = (
        'TTLV'
        'XML'
        'JSON')
      TabOrder = 1
    end
    object GroupBox2: TGroupBox
      Left = 15
      Top = 155
      Width = 410
      Height = 115
      Caption = 'CA Certificate  '
      TabOrder = 4
      object Label3: TLabel
        Left = 15
        Top = 24
        Width = 49
        Height = 13
        Caption = 'File name:'
      end
      object Label4: TLabel
        Left = 15
        Top = 70
        Width = 50
        Height = 13
        Caption = 'Password:'
      end
      object sbBrowseCert: TSpeedButton
        Left = 332
        Top = 40
        Width = 70
        Height = 22
        Caption = 'Browse...'
        OnClick = sbBrowseCertClick
      end
      object edCertFile: TEdit
        Left = 15
        Top = 40
        Width = 311
        Height = 21
        TabOrder = 0
      end
      object edCertPassword: TEdit
        Left = 15
        Top = 85
        Width = 310
        Height = 21
        TabOrder = 1
      end
    end
  end
  object btCancel: TButton
    Left = 360
    Top = 503
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btOkClick
  end
  object OpenDialog: TOpenDialog
    Left = 320
    Top = 104
  end
  object SaveDialog: TSaveDialog
    Left = 392
    Top = 104
  end
end
