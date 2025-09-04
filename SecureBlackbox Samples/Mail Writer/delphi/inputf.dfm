object FormInput: TFormInput
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 101
  ClientWidth = 392
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
  object lblPrompt: TLabel
    Left = 12
    Top = 15
    Width = 34
    Height = 13
    Caption = 'Prompt'
  end
  object edtText: TEdit
    Left = 12
    Top = 34
    Width = 370
    Height = 21
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 226
    Top = 61
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 309
    Top = 61
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
