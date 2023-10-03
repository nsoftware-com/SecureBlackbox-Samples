object FormKMIPclient: TFormKMIPclient
  Left = 300
  Top = 64
  Caption = 'KMIP Client demo'
  ClientHeight = 566
  ClientWidth = 885
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 885
    Height = 80
    Align = alTop
    TabOrder = 0
    object Label10: TLabel
      Left = 8
      Top = 15
      Width = 247
      Height = 13
      Caption = 'This sample illustrates basic KMIP client operations. '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object bRefresh: TButton
      Left = 5
      Top = 49
      Width = 90
      Height = 25
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = bRefreshClick
    end
    object bSettings: TButton
      Left = 100
      Top = 49
      Width = 90
      Height = 25
      Caption = 'Settings'
      TabOrder = 1
      OnClick = bSettingsClick
    end
    object bAddKey: TButton
      Left = 470
      Top = 49
      Width = 90
      Height = 25
      Caption = 'Add key'
      TabOrder = 2
      OnClick = bAddKeyClick
    end
    object bAddCert: TButton
      Left = 565
      Top = 49
      Width = 90
      Height = 25
      Caption = 'Add certificate'
      TabOrder = 3
      OnClick = bAddCertClick
    end
    object bRemove: TButton
      Left = 660
      Top = 49
      Width = 90
      Height = 25
      Caption = 'Remove'
      Enabled = False
      TabOrder = 4
      OnClick = bRemoveClick
    end
    object bCreateCert: TButton
      Left = 375
      Top = 49
      Width = 90
      Height = 25
      Caption = 'Create certificate'
      TabOrder = 5
      OnClick = bCreateCertClick
    end
    object bCreateKey: TButton
      Left = 280
      Top = 49
      Width = 90
      Height = 25
      Caption = 'Create key'
      TabOrder = 6
      OnClick = bCreateKeyClick
    end
  end
  object lvObjects: TListView
    Left = 0
    Top = 80
    Width = 785
    Height = 486
    Align = alClient
    Columns = <
      item
        Caption = 'UniqueIdentifier'
        Width = 150
      end
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Algorithm'
        Width = 130
      end
      item
        Caption = 'Length'
        Width = 70
      end
      item
        Caption = 'Id'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnSelectItem = lvObjectsSelectItem
  end
  object Panel2: TPanel
    Left = 785
    Top = 80
    Width = 100
    Height = 486
    Align = alRight
    TabOrder = 2
    object bDecrypt: TButton
      Left = 5
      Top = 50
      Width = 90
      Height = 25
      Caption = 'Decrypt'
      Enabled = False
      TabOrder = 0
      OnClick = bDecryptClick
    end
    object bEncrypt: TButton
      Left = 5
      Top = 15
      Width = 90
      Height = 25
      Caption = 'Encrypt'
      Enabled = False
      TabOrder = 1
      OnClick = bEncryptClick
    end
    object bSign: TButton
      Left = 5
      Top = 85
      Width = 90
      Height = 25
      Caption = 'Sign'
      Enabled = False
      TabOrder = 2
      OnClick = bSignClick
    end
    object bVerify: TButton
      Left = 5
      Top = 120
      Width = 90
      Height = 25
      Caption = 'Verify'
      Enabled = False
      TabOrder = 3
      OnClick = bVerifyClick
    end
  end
end


