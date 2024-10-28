object FormPasswordVault: TFormPasswordVault
  Left = 311
  Top = 124
  Caption = 'Password vault demo'
  ClientHeight = 397
  ClientWidth = 607
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pTitle: TPanel
    Left = 0
    Top = 0
    Width = 607
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label10: TLabel
      Left = 8
      Top = 15
      Width = 268
      Height = 13
      Caption = 'This sample illustrates basic password vault operations. '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object pMain: TPanel
    Left = 0
    Top = 82
    Width = 607
    Height = 315
    Align = alClient
    TabOrder = 1
    object SplitterVert: TSplitter
      Left = 170
      Top = 1
      Height = 313
      ExplicitTop = 29
      ExplicitHeight = 248
    end
    object pLeft: TPanel
      Left = 1
      Top = 1
      Width = 169
      Height = 313
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object lvEntries: TListView
        Left = 0
        Top = 0
        Width = 169
        Height = 313
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        Columns = <
          item
            Caption = 'Name'
            Width = 150
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = pmEntries
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lvEntriesSelectItem
      end
    end
    object Panel1: TPanel
      Left = 173
      Top = 1
      Width = 433
      Height = 313
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lvValues: TListView
        Left = 0
        Top = 0
        Width = 433
        Height = 313
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        Columns = <
          item
            Caption = 'Name'
            Width = 160
          end
          item
            Caption = 'Value'
            Width = 260
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = pmValues
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 607
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 20
      Height = 13
      Caption = 'Title'
    end
    object Label2: TLabel
      Left = 245
      Top = 14
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object edTitle: TEdit
      Left = 39
      Top = 10
      Width = 178
      Height = 21
      TabOrder = 0
    end
    object edDescription: TEdit
      Left = 310
      Top = 10
      Width = 273
      Height = 21
      TabOrder = 1
    end
  end
  object dlgSave: TSaveDialog
    Left = 520
    Top = 2
  end
  object MainMenu1: TMainMenu
    Left = 309
    Top = 3
    object miVault: TMenuItem
      Caption = 'Vault'
      object miNew: TMenuItem
        Caption = 'New'
        OnClick = miNewClick
      end
      object miOpen: TMenuItem
        Caption = 'Open...'
        OnClick = miOpenClick
      end
      object miSave: TMenuItem
        Caption = 'Save...'
        OnClick = miSaveClick
      end
      object miClose: TMenuItem
        Caption = 'Close'
        OnClick = miCloseClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'Exit'
        OnClick = miExitClick
      end
    end
  end
  object pmEntries: TPopupMenu
    OnPopup = pmEntriesPopup
    Left = 93
    Top = 187
    object miAdd: TMenuItem
      Caption = 'Add'
      OnClick = miAddClick
    end
    object miRemove: TMenuItem
      Caption = 'Remove'
      Enabled = False
      OnClick = miRemoveClick
    end
    object miRemoveAll: TMenuItem
      Caption = 'RemoveAll'
      Enabled = False
      OnClick = miRemoveAllClick
    end
    object miChangekey: TMenuItem
      Caption = 'Change key'
      Enabled = False
      OnClick = miChangekeyClick
    end
  end
  object dlgOpen: TOpenDialog
    Left = 429
    Top = 3
  end
  object pmValues: TPopupMenu
    OnPopup = pmValuesPopup
    Left = 373
    Top = 195
    object miAdd1: TMenuItem
      Caption = 'Add'
      OnClick = miAdd1Click
    end
    object miModify1: TMenuItem
      Caption = 'Modify'
      Enabled = False
      OnClick = miModify1Click
    end
    object miRemove1: TMenuItem
      Caption = 'Remove'
      Enabled = False
      OnClick = miRemove1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miRefresh1: TMenuItem
      Caption = 'Refresh value'
      OnClick = miRefresh1Click
    end
  end
end


