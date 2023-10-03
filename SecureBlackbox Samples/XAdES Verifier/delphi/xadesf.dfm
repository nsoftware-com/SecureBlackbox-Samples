object FormXades: TFormXades
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'XAdES Options'
  ClientHeight = 219
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
    219)
  PixelsPerInch = 96
  TextHeight = 13
  object lbSignedTime: TLabel
    Left = 8
    Top = 98
    Width = 61
    Height = 13
    Caption = 'Signed Time:'
  end
  object lbTimestamp: TLabel
    Left = 8
    Top = 122
    Width = 55
    Height = 13
    Caption = 'Timestamp:'
  end
  object lbTimestampSerial: TLabel
    Left = 8
    Top = 146
    Width = 84
    Height = 13
    Caption = 'Timestamp Serial:'
  end
  object lbVersion: TLabel
    Left = 8
    Top = 23
    Width = 39
    Height = 13
    Caption = 'Version:'
  end
  object Label2: TLabel
    Left = 19
    Top = 58
    Width = 28
    Height = 13
    Caption = 'Form:'
  end
  object btnOK: TButton
    Left = 152
    Top = 187
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object edVersion: TEdit
    Left = 53
    Top = 20
    Width = 204
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 1
  end
  object edForm: TEdit
    Left = 53
    Top = 55
    Width = 204
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 2
  end
end