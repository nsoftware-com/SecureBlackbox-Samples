object FormPrompt: TFormPrompt
  Left = 472
  Top = 447
  BorderStyle = bsDialog
  Caption = 'Keyboard authentication'
  ClientHeight = 100
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 13
    Caption = 'lblPrompt'
  end
  object edtResponse: TEdit
    Left = 8
    Top = 32
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 40
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 136
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
