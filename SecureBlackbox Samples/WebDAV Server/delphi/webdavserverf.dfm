object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'SecureBlackbox WebDAV Server Demo'
  ClientHeight = 585
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 449
    Height = 401
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
      Top = 91
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = sbChooseFileDirClick
    end
    object Label3: TLabel
      Left = 16
      Top = 184
      Width = 83
      Height = 13
      Caption = 'X.509 certificate:'
    end
    object sbChooseCert: TSpeedButton
      Left = 407
      Top = 203
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = sbChooseCertClick
    end
    object Label4: TLabel
      Left = 16
      Top = 240
      Width = 103
      Height = 13
      Caption = 'Certificate password:'
    end
    object Label5: TLabel
      Left = 16
      Top = 296
      Width = 47
      Height = 13
      Caption = 'Users list:'
    end
    object sbUserAdd: TSpeedButton
      Left = 407
      Top = 315
      Width = 23
      Height = 22
      Caption = '+'
      OnClick = sbUserAddClick
    end
    object sbUserDelete: TSpeedButton
      Left = 407
      Top = 343
      Width = 23
      Height = 22
      Caption = '-'
      OnClick = sbUserDeleteClick
    end
    object Label6: TLabel
      Left = 16
      Top = 128
      Width = 81
      Height = 13
      Caption = 'Metadata folder:'
    end
    object sbChooseMetaDir: TSpeedButton
      Left = 407
      Top = 146
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = sbChooseMetaDirClick
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
      Top = 203
      Width = 385
      Height = 21
      TabOrder = 3
    end
    object edCertPassword: TEdit
      Left = 16
      Top = 259
      Width = 385
      Height = 21
      PasswordChar = '*'
      TabOrder = 4
    end
    object lvUsers: TListView
      Left = 16
      Top = 315
      Width = 385
      Height = 70
      Columns = <
        item
          Caption = 'Login'
          Width = 150
        end
        item
          Caption = 'Password'
          Width = 150
        end>
      RowSelect = True
      TabOrder = 5
      ViewStyle = vsReport
    end
    object edMetaDir: TEdit
      Left = 16
      Top = 147
      Width = 385
      Height = 21
      TabOrder = 6
    end
  end
  object bbStart: TButton
    Left = 151
    Top = 544
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = bbStartClick
  end
  object bbStop: TButton
    Left = 232
    Top = 544
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = bbStopClick
  end
  object mmLog: TMemo
    Left = 8
    Top = 415
    Width = 449
    Height = 114
    TabOrder = 3
  end
  object FolderOpenDlg: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist]
    Left = 352
    Top = 24
  end
  object OpenFileDlg: TOpenDialog
    Left = 424
    Top = 23
  end
end


