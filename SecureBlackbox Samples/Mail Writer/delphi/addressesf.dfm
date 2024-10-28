object FormAddresses: TFormAddresses
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '%s Addresses'
  ClientHeight = 274
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblAddress: TLabel
    Left = 214
    Top = 8
    Width = 74
    Height = 13
    Caption = 'E-mail Address:'
  end
  object lblAddresses: TLabel
    Left = 8
    Top = 72
    Width = 54
    Height = 13
    Caption = 'Addresses:'
  end
  object edtName: TEdit
    Left = 8
    Top = 27
    Width = 200
    Height = 21
    TabOrder = 0
  end
  object lvwAddresses: TListView
    Left = 8
    Top = 91
    Width = 407
    Height = 146
    Columns = <
      item
        Caption = 'Name'
        Width = 193
      end
      item
        Caption = 'Address'
        Width = 193
      end>
    ColumnClick = False
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
  end
  object edtAddress: TEdit
    Left = 214
    Top = 27
    Width = 201
    Height = 21
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 340
    Top = 54
    Width = 75
    Height = 23
    Caption = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 8
    Top = 243
    Width = 75
    Height = 23
    Caption = 'Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object btnOK: TButton
    Left = 259
    Top = 243
    Width = 75
    Height = 23
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 340
    Top = 243
    Width = 75
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
