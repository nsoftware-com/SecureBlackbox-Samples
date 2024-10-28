object FormHttppost: TFormHttppost
  Left = 346
  Top = 177
  BorderStyle = bsDialog
  Caption = 'HTTP Post demo'
  ClientHeight = 390
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 47
    Width = 23
    Height = 13
    Caption = 'URL:'
  end
  object Label5: TLabel
    Left = 10
    Top = 83
    Width = 20
    Height = 13
    Caption = 'File:'
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 345
    Height = 13
    Caption = 
      'This sample illustrates POST request capabilities of HTTPClient' +
      ' control. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object EdURL: TEdit
    Left = 39
    Top = 43
    Width = 370
    Height = 21
    TabOrder = 0
    Text = 'http://localhost/upload/simple_upload.php'
  end
  object btnGo: TButton
    Left = 334
    Top = 130
    Width = 75
    Height = 25
    Caption = 'Post'
    Default = True
    TabOrder = 1
    OnClick = btnGoClick
  end
  object MMLog: TMemo
    Left = 0
    Top = 162
    Width = 414
    Height = 228
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object EdFileName: TEdit
    Left = 39
    Top = 79
    Width = 282
    Height = 21
    TabOrder = 3
  end
  object BtSel: TButton
    Left = 334
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    TabOrder = 4
    OnClick = BtSelClick
  end
  object cbPostAsForm: TCheckBox
    Left = 238
    Top = 134
    Width = 90
    Height = 17
    Caption = 'Post as form'
    TabOrder = 5
  end
  object dlgOpen: TOpenDialog
    Left = 360
    Top = 8
  end
end


