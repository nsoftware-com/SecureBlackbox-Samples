object FormOperation: TFormOperation
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'FormOperation'
  ClientHeight = 211
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    464
    211)
  PixelsPerInch = 96
  TextHeight = 13
  object Label13: TLabel
    Left = 8
    Top = 16
    Width = 73
    Height = 13
    Caption = 'Input filename:'
  end
  object lOutputfile: TLabel
    Left = 8
    Top = 72
    Width = 81
    Height = 13
    Caption = 'Output filename:'
  end
  object lHashAlgorithm: TLabel
    Left = 9
    Top = 131
    Width = 75
    Height = 13
    Caption = 'Hash algorithm:'
  end
  object lIV: TLabel
    Left = 8
    Top = 132
    Width = 14
    Height = 13
    Caption = 'IV:'
  end
  object eInputFile: TEdit
    Left = 8
    Top = 36
    Width = 361
    Height = 21
    TabOrder = 0
  end
  object eOutputFile: TEdit
    Left = 8
    Top = 88
    Width = 361
    Height = 21
    TabOrder = 1
  end
  object cbHashAlgorithm: TComboBox
    Left = 90
    Top = 128
    Width = 118
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 2
    Text = 'SHA1'
    Items.Strings = (
      'SHA1'
      'MD5'
      'MD2'
      'SHA256'
      'SHA384'
      'SHA512')
  end
  object bDo: TButton
    Left = 290
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Sign'
    TabOrder = 3
    OnClick = bDoClick
  end
  object bOutputFileSign: TButton
    Left = 377
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 4
    OnClick = bOutputFileSignClick
  end
  object bInputFileSign: TButton
    Left = 377
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 5
    OnClick = bInputFileSignClick
  end
  object bClose: TButton
    Left = 377
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 6
  end
  object eIV: TEdit
    Left = 24
    Top = 128
    Width = 169
    Height = 21
    TabOrder = 7
  end
  object dlgSaveFile: TSaveDialog
    Left = 236
    Top = 3
  end
  object dlgOpenFile: TOpenDialog
    Left = 148
    Top = 3
  end
end
