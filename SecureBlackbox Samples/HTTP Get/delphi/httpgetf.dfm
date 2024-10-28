object FormHttpget: TFormHttpget
  Left = 346
  Top = 177
  BorderStyle = bsDialog
  Caption = 'HTTP Get demo'
  ClientHeight = 443
  ClientWidth = 433
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
    Top = 15
    Width = 358
    Height = 13
    Caption = 
      'This sample illustrates the ways of making GET requests with HTT' +
      'PSClient. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnGo: TButton
    Left = 264
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Get'
    Default = True
    TabOrder = 0
    OnClick = btnGoClick
  end
  object MMLog: TMemo
    Left = 0
    Top = 224
    Width = 433
    Height = 219
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object btnHead: TButton
    Left = 350
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Head'
    TabOrder = 2
    OnClick = btnHeadClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 417
    Height = 130
    Caption = 'Http options  '
    TabOrder = 3
    object Label1: TLabel
      Left = 15
      Top = 50
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object Label2: TLabel
      Left = 15
      Top = 25
      Width = 43
      Height = 13
      Caption = 'Protocol:'
    end
    object Label3: TLabel
      Left = 212
      Top = 50
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object Label4: TLabel
      Left = 15
      Top = 75
      Width = 26
      Height = 13
      Caption = 'Path:'
    end
    object Label5: TLabel
      Left = 15
      Top = 105
      Width = 41
      Height = 13
      Caption = 'Save to:'
    end
    object EdHost: TEdit
      Left = 74
      Top = 46
      Width = 129
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object CbProtocol: TComboBox
      Left = 74
      Top = 21
      Width = 95
      Height = 21
      TabOrder = 1
      Text = 'HTTP'
      Items.Strings = (
        'HTTP'
        'HTTPS')
    end
    object EdPort: TEdit
      Left = 240
      Top = 46
      Width = 91
      Height = 21
      TabOrder = 2
      Text = '80'
    end
    object EdPath: TEdit
      Left = 74
      Top = 71
      Width = 257
      Height = 21
      TabOrder = 3
      Text = '/'
    end
    object EdFileName: TEdit
      Left = 74
      Top = 101
      Width = 257
      Height = 21
      TabOrder = 4
    end
    object BtSel: TButton
      Left = 336
      Top = 99
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      TabOrder = 5
      OnClick = BtSelClick
    end
  end
  object dlgSave: TSaveDialog
    Left = 466
    Top = 8
  end
end


