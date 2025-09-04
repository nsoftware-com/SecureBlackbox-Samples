object FormReferences: TFormReferences
  Left = 566
  Top = 461
  BorderStyle = bsDialog
  Caption = 'References'
  ClientHeight = 229
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    324
    229)
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 241
    Top = 199
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 243
    ExplicitTop = 197
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
