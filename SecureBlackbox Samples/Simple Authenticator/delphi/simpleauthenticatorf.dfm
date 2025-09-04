object FormSimpleAuthenticator: TFormSimpleAuthenticator
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple authenticator demo'
  ClientHeight = 144
  ClientWidth = 348
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
    Left = 71
    Top = 54
    Width = 39
    Height = 13
    Caption = 'User Id:'
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 216
    Height = 13
    Caption = 'This sample shows how to use authenticator.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object eUserId: TEdit
    Left = 127
    Top = 51
    Width = 146
    Height = 21
    TabOrder = 0
  end
  object bStart: TButton
    Left = 141
    Top = 100
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = bStartClick
  end
end


