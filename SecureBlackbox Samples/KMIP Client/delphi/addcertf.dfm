object FormAddCert: TFormAddCert
  Left = 472
  Top = 447
  BorderStyle = bsDialog
  Caption = 'Adding certificate'
  ClientHeight = 178
  ClientWidth = 446
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
  object Label1: TLabel
    Left = 15
    Top = 104
    Width = 11
    Height = 13
    Caption = 'ID'
  end
  object Label2: TLabel
    Left = 15
    Top = 26
    Width = 63
    Height = 13
    Caption = 'Certificate file'
  end
  object Label5: TLabel
    Left = 15
    Top = 64
    Width = 98
    Height = 13
    Caption = 'Certificate password:'
  end
  object btnOk: TButton
    Left = 272
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 361
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edCertFile: TEdit
    Left = 84
    Top = 20
    Width = 271
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object bOpenKeyFile: TButton
    Left = 361
    Top = 20
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = bOpenKeyFileClick
  end
  object edCertId: TEdit
    Left = 40
    Top = 100
    Width = 145
    Height = 21
    TabOrder = 4
  end
  object edCertPassword: TEdit
    Left = 130
    Top = 60
    Width = 199
    Height = 21
    PasswordChar = '*'
    TabOrder = 5
  end
  object dlgOpenFile: TOpenDialog
    Left = 410
    Top = 51
  end
end
