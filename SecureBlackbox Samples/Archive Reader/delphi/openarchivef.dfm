object FormOpenarchive: TFormOpenarchive
  Left = 799
  Top = 216
  BorderStyle = bsSingle
  Caption = 'Open archive'
  ClientHeight = 141
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblArchiveName: TLabel
    Left = 15
    Top = 23
    Width = 55
    Height = 13
    Caption = 'Archive file:'
  end
  object lblArchiveType: TLabel
    Left = 8
    Top = 59
    Width = 62
    Height = 13
    Caption = 'Archive type:'
  end
  object edtArchiveFile: TEdit
    Left = 80
    Top = 20
    Width = 345
    Height = 21
    TabOrder = 0
  end
  object btnChoose: TButton
    Left = 430
    Top = 20
    Width = 80
    Height = 25
    Caption = 'Choose...'
    TabOrder = 1
    OnClick = btnChooseClick
  end
  object btnCancel: TButton
    Left = 430
    Top = 104
    Width = 80
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 340
    Top = 104
    Width = 80
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
  end
  object cbArchiveType: TComboBox
    Left = 80
    Top = 56
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = 'Zip'
    Items.Strings = (
      'Zip'
      'Tar Gzip'
      'Tar Bzip2'
      'Gzip'
      'Bzip2')
  end
  object odArchiveFile: TOpenDialog
    Left = 232
    Top = 88
  end
end
