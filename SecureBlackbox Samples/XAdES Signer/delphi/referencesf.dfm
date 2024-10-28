object FormReferences: TFormReferences
  Left = 566
  Top = 461
  BorderStyle = bsDialog
  Caption = 'References'
  ClientHeight = 237
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    296
    237)
  PixelsPerInch = 96
  TextHeight = 13
  object lbReferences: TListBox
    Left = 8
    Top = 8
    Width = 195
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnClose: TButton
    Left = 212
    Top = 197
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 212
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 212
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object btnEdit: TButton
    Left = 212
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 4
    OnClick = btnEditClick
  end
end
