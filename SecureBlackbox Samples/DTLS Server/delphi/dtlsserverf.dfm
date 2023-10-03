object FormDtlsServer: TFormDtlsServer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DTLS Server Demo'
  ClientHeight = 639
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    529
    639)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 513
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample is a very simple echo server that shows how to use t' +
      'he DTLSServer component to receive messages from DTLS clients an' +
      'd send them back to the clients.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object grpSettings: TGroupBox
    Left = 8
    Top = 40
    Width = 513
    Height = 85
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Server Settings '
    TabOrder = 0
    DesignSize = (
      513
      85)
    object lblPort: TLabel
      Left = 249
      Top = 50
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object lblCertificate: TLabel
      Left = 15
      Top = 19
      Width = 54
      Height = 13
      Caption = 'Certificate:'
    end
    object lblPassword: TLabel
      Left = 19
      Top = 50
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object edtPort: TEdit
      Left = 279
      Top = 47
      Width = 40
      Height = 21
      TabOrder = 3
      Text = '4433'
    end
    object udPort: TUpDown
      Left = 319
      Top = 47
      Width = 16
      Height = 21
      Associate = edtPort
      Min = 1
      Max = 65535
      Position = 4433
      TabOrder = 4
      Thousands = False
    end
    object btnStart: TButton
      Left = 344
      Top = 45
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Start'
      TabOrder = 5
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 425
      Top = 45
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Stop'
      TabOrder = 6
      OnClick = btnStopClick
    end
    object edtFilename: TEdit
      Left = 75
      Top = 16
      Width = 394
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnBrowse: TButton
      Left = 475
      Top = 14
      Width = 25
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object edtPassword: TEdit
      Left = 75
      Top = 47
      Width = 157
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
  end
  object grpLog: TGroupBox
    Left = 8
    Top = 131
    Width = 513
    Height = 500
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Server Log '
    TabOrder = 1
    DesignSize = (
      513
      500)
    object lvwLog: TListView
      Left = 12
      Top = 19
      Width = 488
      Height = 470
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Time'
          Width = 60
        end
        item
          AutoSize = True
          Caption = 'Event'
        end>
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object dlgOpenCertificate: TOpenDialog
    Filter = 'Certificates (*.pfx)|*.pfx'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open Server Certificate'
    Left = 41
    Top = 558
  end
end


