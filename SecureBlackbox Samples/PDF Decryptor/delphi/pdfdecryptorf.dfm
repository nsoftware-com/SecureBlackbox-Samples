object FormPdfdecryptor: TFormPdfdecryptor
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'PDF Decryptor demo'
  ClientHeight = 178
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lSourceFile: TLabel
    Left = 7
    Top = 56
    Width = 76
    Height = 13
    Caption = 'Source PDF file:'
  end
  object lDestFile: TLabel
    Left = 7
    Top = 98
    Width = 97
    Height = 13
    Caption = 'Destination PDF file:'
  end
  object lDemoInfo: TLabel
    Left = 7
    Top = 7
    Width = 312
    Height = 42
    AutoSize = False
    Caption = 
      'This sample illustrates the use of PDFDecryptor component for de' +
      'crypting PDF documents. Please pick the PDF document and then cl' +
      'ick '#39'Decrypt'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object editSource: TEdit
    Left = 7
    Top = 70
    Width = 242
    Height = 21
    TabOrder = 0
  end
  object editDest: TEdit
    Left = 7
    Top = 111
    Width = 242
    Height = 21
    TabOrder = 2
  end
  object btnBrowseSource: TButton
    Left = 255
    Top = 70
    Width = 65
    Height = 21
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseSourceClick
  end
  object btnBrowseDest: TButton
    Left = 255
    Top = 111
    Width = 65
    Height = 22
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btnBrowseDestClick
  end
  object btnDecrypt: TButton
    Left = 90
    Top = 144
    Width = 64
    Height = 21
    Caption = 'Decrypt'
    Default = True
    TabOrder = 4
    OnClick = btnDecryptClick
  end
  object btnCancel: TButton
    Left = 159
    Top = 144
    Width = 65
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object OpenDialogPDF: TOpenDialog
    InitialDir = '.'
    Left = 272
    Top = 80
  end
  object SaveDialogPDF: TSaveDialog
    InitialDir = '.'
    Left = 272
    Top = 128
  end
end


