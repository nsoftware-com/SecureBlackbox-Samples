object FormMessageverifier: TFormMessageverifier
  Left = 0
  Top = 0
  Caption = 'Message Verifier demo'
  ClientHeight = 336
  ClientWidth = 458
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
  object lbXMLFile: TLabel
    Left = 8
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 366
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object lOutputFile: TLabel
    Left = 8
    Top = 111
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbBrowseOutputFile: TSpeedButton
    Left = 366
    Top = 105
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseOutputFileClick
  end
  object Label10: TLabel
    Left = 5
    Top = 15
    Width = 448
    Height = 13
    Caption = 
      'This sample showcases MessageVerifier'#39's facilities in validating' +
      ' PKCS#7-compliant signed files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object cbDetached: TCheckBox
    Left = 8
    Top = 84
    Width = 209
    Height = 17
    Caption = 'Detached'
    TabOrder = 0
    OnClick = cbDetachedClick
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 285
    Height = 21
    TabOrder = 1
  end
  object btnVerify: TButton
    Left = 366
    Top = 301
    Width = 75
    Height = 25
    Caption = 'Verify'
    TabOrder = 2
    OnClick = btnVerifyClick
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 107
    Width = 285
    Height = 21
    TabOrder = 3
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 150
    Width = 433
    Height = 135
    Caption = 'Known Certificates'
    TabOrder = 4
    object lvKnownCertificates: TListView
      Left = 5
      Top = 24
      Width = 335
      Height = 100
      Columns = <
        item
          Caption = 'Serial'
          Width = 150
        end
        item
          Caption = 'Issuer'
          Width = 170
        end>
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object btnRemoveKnown: TButton
      Left = 350
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveKnownClick
    end
    object btnAddKnown: TButton
      Left = 350
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 2
      OnClick = btnAddKnownClick
    end
  end
  object dlgOpen: TOpenDialog
    Left = 241
    Top = 32
  end
  object dlgSave: TSaveDialog
    Left = 304
    Top = 32
  end
end


