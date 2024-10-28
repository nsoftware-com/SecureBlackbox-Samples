object FormResults: TFormResults
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Decryption Results'
  ClientHeight = 385
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    372
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 152
    Top = 353
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object mResults: TMemo
    Left = 8
    Top = 8
    Width = 356
    Height = 329
    TabOrder = 1
  end
end
