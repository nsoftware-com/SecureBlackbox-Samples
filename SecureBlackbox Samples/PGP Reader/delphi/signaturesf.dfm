object FormSignatures: TFormSignatures
  Left = 310
  Top = 272
  Caption = 'Signatures'
  ClientHeight = 235
  ClientWidth = 433
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
  object lvSignatures: TListView
    Left = 0
    Top = 0
    Width = 433
    Height = 235
    Align = alClient
    Columns = <
      item
        Caption = 'Signer'
        Width = 200
      end
      item
        Caption = 'Validity'
        Width = 200
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
end
