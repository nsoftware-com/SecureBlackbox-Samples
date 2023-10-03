object FormEditValue: TFormEditValue
  Left = 299
  Top = 124
  BorderStyle = bsDialog
  ClientHeight = 177
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 27
    Width = 55
    Height = 13
    Caption = 'Field name:'
  end
  object Label1: TLabel
    Left = 33
    Top = 67
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object cbEncrypted: TCheckBox
    Left = 69
    Top = 104
    Width = 89
    Height = 17
    Caption = 'Encrypted'
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 198
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 278
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object edFieldName: TEdit
    Left = 69
    Top = 24
    Width = 204
    Height = 21
    TabOrder = 0
  end
  object edValue: TEdit
    Left = 69
    Top = 64
    Width = 284
    Height = 21
    TabOrder = 1
  end
end
