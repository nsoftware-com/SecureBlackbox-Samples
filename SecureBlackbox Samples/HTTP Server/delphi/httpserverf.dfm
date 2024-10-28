object FormHttpserver: TFormHttpserver
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SecureBlackbox HTTP Server Demo'
  ClientHeight = 496
  ClientWidth = 464
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
    Width = 378
    Height = 13
    Caption = 
      'This sample is a basic HTTP server. Tune up the server component' +
      ' as needed, '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 27
    Width = 214
    Height = 13
    Caption = 'and then press Start to activate the server. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 56
    Width = 449
    Height = 250
    Caption = 'Server Options'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 32
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object Label2: TLabel
      Left = 16
      Top = 72
      Width = 56
      Height = 13
      Caption = 'Files folder:'
    end
    object sbChooseFileDir: TSpeedButton
      Left = 407
      Top = 90
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = sbChooseFileDirClick
    end
    object Label3: TLabel
      Left = 16
      Top = 128
      Width = 83
      Height = 13
      Caption = 'X.509 certificate:'
    end
    object SpeedButton1: TSpeedButton
      Left = 407
      Top = 146
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object Label4: TLabel
      Left = 16
      Top = 184
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object edPort: TEdit
      Left = 62
      Top = 29
      Width = 163
      Height = 21
      TabOrder = 0
      Text = '8080'
    end
    object cbUseTLS: TCheckBox
      Left = 240
      Top = 31
      Width = 97
      Height = 17
      Caption = 'Use SSL/TLS'
      TabOrder = 1
    end
    object edFileDir: TEdit
      Left = 16
      Top = 91
      Width = 385
      Height = 21
      TabOrder = 2
    end
    object edCertFile: TEdit
      Left = 16
      Top = 147
      Width = 385
      Height = 21
      TabOrder = 3
    end
    object edCertPassword: TEdit
      Left = 16
      Top = 203
      Width = 385
      Height = 21
      PasswordChar = '*'
      TabOrder = 4
    end
  end
  object bbStart: TButton
    Left = 158
    Top = 460
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = bbStartClick
  end
  object bbStop: TButton
    Left = 239
    Top = 460
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = bbStopClick
  end
  object mmLog: TMemo
    Left = 8
    Top = 312
    Width = 451
    Height = 137
    TabOrder = 3
  end
  object FolderOpenDlg: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist]
    Left = 424
    Top = 80
  end
  object OpenFileDlg: TOpenDialog
    Left = 368
    Top = 79
  end
end


