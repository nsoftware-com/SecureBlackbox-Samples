object FormOTPServer: TFormOTPServer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OTP Server Demo'
  ClientHeight = 366
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 310
    Height = 13
    Caption = 'This sample acts as a basic One-Time-Password protocol server. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 38
    Width = 362
    Height = 239
    Caption = 'Security parameters  '
    TabOrder = 0
    object Label2: TLabel
      Left = 40
      Top = 34
      Width = 56
      Height = 13
      Caption = 'Key Secret:'
      Transparent = True
    end
    object Label3: TLabel
      Left = 14
      Top = 62
      Width = 83
      Height = 13
      Caption = 'Password length:'
      Transparent = True
    end
    object Label1: TLabel
      Left = 47
      Top = 94
      Width = 49
      Height = 13
      Caption = 'Algorithm:'
      Transparent = True
    end
    object eKeySecret: TEdit
      Left = 105
      Top = 31
      Width = 247
      Height = 21
      TabOrder = 0
    end
    object ePassLen: TEdit
      Left = 105
      Top = 58
      Width = 44
      Height = 21
      TabOrder = 1
      Text = '10'
    end
    object cbAlgorithm: TComboBox
      Left = 105
      Top = 91
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Hmac'
      OnChange = cbAlgorithmChange
      Items.Strings = (
        'Hmac'
        'Time')
    end
    object GroupBox2: TGroupBox
      Left = 10
      Top = 128
      Width = 342
      Height = 105
      TabOrder = 3
      object lInterval: TLabel
        Left = 22
        Top = 18
        Width = 65
        Height = 13
        Caption = 'Time interval:'
        Transparent = True
      end
      object lHashAlgorithm: TLabel
        Left = 12
        Top = 46
        Width = 75
        Height = 13
        Caption = 'Hash algorithm:'
        Transparent = True
      end
      object eInterval: TEdit
        Left = 95
        Top = 15
        Width = 51
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object cbHashAlgorithm: TComboBox
        Left = 95
        Top = 42
        Width = 203
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'SHA1 (default)'
        Items.Strings = (
          'SHA1 (default)'
          'SHA256'
          'SHA512')
      end
      object cUseBaseTime: TCheckBox
        Left = 5
        Top = 74
        Width = 82
        Height = 17
        Caption = ' Use this time'
        TabOrder = 2
      end
      object eBaseDate: TDateTimePicker
        Left = 95
        Top = 72
        Width = 83
        Height = 21
        Date = 0.573851631947036400
        Time = 0.573851631947036400
        TabOrder = 3
      end
      object eBaseTime: TDateTimePicker
        Left = 184
        Top = 72
        Width = 75
        Height = 21
        Date = 42047.565731145820000000
        Time = 42047.565731145820000000
        Kind = dtkTime
        TabOrder = 4
      end
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 283
    Width = 362
    Height = 70
    TabOrder = 1
    object Label4: TLabel
      Left = 14
      Top = 41
      Width = 50
      Height = 13
      Caption = 'Password:'
      Transparent = True
    end
    object Label5: TLabel
      Left = 35
      Top = 14
      Width = 29
      Height = 13
      Caption = 'Delta:'
      Transparent = True
    end
    object bValidate: TButton
      Left = 242
      Top = 34
      Width = 110
      Height = 25
      Caption = 'Is password valid'
      TabOrder = 0
      OnClick = bValidateClick
    end
    object ePassword: TEdit
      Left = 70
      Top = 38
      Width = 155
      Height = 21
      TabOrder = 1
    end
    object eDelta: TEdit
      Left = 70
      Top = 11
      Width = 51
      Height = 21
      TabOrder = 2
      Text = '0'
    end
  end
end


