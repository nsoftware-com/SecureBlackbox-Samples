object FormReferences: TFormReferences
  Left = 566
  Top = 461
  Caption = 'References'
  ClientHeight = 227
  ClientWidth = 326
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
  object btnClose: TButton
    Left = 243
    Top = 197
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
  object lvReferenceResults: TListView
    Left = 8
    Top = 8
    Width = 310
    Height = 177
    Columns = <
      item
        Caption = 'Id'
        Width = 80
      end
      item
        Caption = 'Uri'
        Width = 150
      end
      item
        Caption = 'DigestValid'
        Width = 65
      end>
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
end