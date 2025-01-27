object FormCertificate: TFormCertificate
  Left = 685
  Top = 322
  BorderStyle = bsDialog
  Caption = 'Private key needed'
  ClientHeight = 97
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblCertFile: TLabel
    Left = 8
    Top = 11
    Width = 66
    Height = 13
    Caption = 'Certificate file:'
  end
  object lblCertificatePassword: TLabel
    Left = 8
    Top = 38
    Width = 98
    Height = 13
    Caption = 'Certificate password:'
  end
  object edtCertFile: TEdit
    Left = 88
    Top = 7
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object btnChooseFile: TButton
    Left = 216
    Top = 7
    Width = 65
    Height = 21
    Caption = 'Choose...'
    TabOrder = 1
    OnClick = btnChooseFileClick
  end
  object edtCertPassword: TEdit
    Left = 112
    Top = 34
    Width = 169
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
    OnKeyPress = edtCertPasswordKeyPress
  end
  object btnLoad: TButton
    Left = 64
    Top = 67
    Width = 75
    Height = 25
    Caption = 'Load'
    Default = True
    TabOrder = 3
    OnClick = btnLoadClick
  end
  object btnCancel: TButton
    Left = 144
    Top = 67
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object odOpenCertificate: TOpenDialog
    Filter = 'PFX certificates (*.pfx)|*.pfx'
    Left = 8
    Top = 64
  end
end
