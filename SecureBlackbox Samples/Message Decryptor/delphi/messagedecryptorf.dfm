object FormMessagedecryptor: TFormMessagedecryptor
  Left = 0
  Top = 0
  Caption = 'Message Decryptor demo'
  ClientHeight = 289
  ClientWidth = 395
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
  object lbInputFile: TLabel
    Left = 8
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseInputFile: TSpeedButton
    Left = 303
    Top = 43
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseInputFileClick
  end
  object lOutputFile: TLabel
    Left = 8
    Top = 84
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 303
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 381
    Height = 13
    Caption = 
      'This sample performs decryption of encrypted ('#39'enveloped'#39') PKCS#' +
      '7 messages.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edInputFile: TEdit
    Left = 70
    Top = 45
    Width = 225
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 70
    Top = 80
    Width = 225
    Height = 21
    TabOrder = 1
  end
  object btnDecrypt: TButton
    Left = 303
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Decrypt'
    TabOrder = 2
    OnClick = btnDecryptClick
  end
  object gbSigningCertificates: TGroupBox
    Left = 8
    Top = 117
    Width = 370
    Height = 122
    Caption = 'Certificates '
    TabOrder = 3
    object lvCertificates: TListView
      Left = 8
      Top = 24
      Width = 275
      Height = 90
      Columns = <
        item
          Caption = 'Serial'
          Width = 110
        end
        item
          Caption = 'Issuer'
          Width = 150
        end>
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object btnRemove: TButton
      Left = 290
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveClick
    end
    object btnAdd: TButton
      Left = 290
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 2
      OnClick = btnAddClick
    end
  end
  object dlgOpenFile: TOpenDialog
    Left = 112
    Top = 32
  end
  object dlgSaveFile: TSaveDialog
    Left = 216
    Top = 32
  end
  object DlgOpen: TOpenDialog
    Filter = 
      'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*' +
      '.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX'
    Left = 232
    Top = 184
  end
end


