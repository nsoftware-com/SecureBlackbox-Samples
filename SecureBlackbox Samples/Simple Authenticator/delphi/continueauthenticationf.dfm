object FormContinueAuthentication: TFormContinueAuthentication
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Continue authentication'
  ClientHeight = 141
  ClientWidth = 343
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
  object Label2: TLabel
    Left = 38
    Top = 22
    Width = 66
    Height = 13
    Caption = 'Auth method:'
  end
  object Label3: TLabel
    Left = 47
    Top = 64
    Width = 57
    Height = 13
    Caption = 'Auth token:'
  end
  object eAuthMethod: TEdit
    Left = 110
    Top = 19
    Width = 154
    Height = 21
    Enabled = False
    TabOrder = 0
  end
  object bContinue: TButton
    Left = 158
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Continue'
    TabOrder = 1
    OnClick = bContinueClick
  end
  object eAuthToken: TEdit
    Left = 110
    Top = 61
    Width = 186
    Height = 21
    TabOrder = 2
  end
  object bClose: TButton
    Left = 246
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 3
    OnClick = bContinueClick
  end
end
