object FormPassword: TFormPassword
  Left = 528
  Top = 354
  BorderStyle = bsDialog
  Caption = 'Password request'
  ClientHeight = 72
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCanResize = FormCanResize
  PixelsPerInch = 96
  TextHeight = 13
  object lblPasswordNeeded: TLabel
    Left = 8
    Top = 12
    Width = 110
    Height = 13
    Caption = 'Please enter password:'
  end
  object edtPassword: TEdit
    Left = 129
    Top = 8
    Width = 169
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
    OnKeyPress = edtPasswordKeyPress
  end
  object btnOk: TButton
    Left = 79
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 160
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
