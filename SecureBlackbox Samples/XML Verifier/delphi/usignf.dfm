object FormUsign: TFormUsign
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Signature Info'
  ClientHeight = 380
  ClientWidth = 324
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
  object Label10: TLabel
    Left = 8
    Top = 12
    Width = 235
    Height = 13
    Caption = 'Please provide certificate for signature validation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object gbGeneralEnc: TGroupBox
    Left = 8
    Top = 31
    Width = 308
    Height = 115
    Caption = 'General'
    TabOrder = 0
    DesignSize = (
      308
      115)
    object lbHashAlgorithm: TLabel
      Left = 16
      Top = 62
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lbCanonMethod: TLabel
      Left = 16
      Top = 23
      Width = 77
      Height = 26
      Caption = 'Canonicalization'#13#10'method:'
    end
    object edCanonMethod: TEdit
      Left = 99
      Top = 26
      Width = 165
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 1
    end
    object edHashAlgorithm: TEdit
      Left = 99
      Top = 59
      Width = 196
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 0
    end
  end
  object gbKeyInfo: TGroupBox
    Left = 8
    Top = 152
    Width = 308
    Height = 184
    Caption = 'Key Info'
    TabOrder = 1
    DesignSize = (
      308
      184)
    object lbKeyName: TLabel
      Left = 16
      Top = 28
      Width = 52
      Height = 13
      Caption = 'Key Name:'
    end
    object edKeyName: TEdit
      Left = 80
      Top = 24
      Width = 213
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 0
    end
    object gbSigningCertificates: TGroupBox
      Left = 3
      Top = 53
      Width = 302
      Height = 122
      Caption = 'Known Certificates'
      TabOrder = 1
      object lvKnownCertificates: TListView
        Left = 8
        Top = 24
        Width = 210
        Height = 90
        Columns = <
          item
            Caption = 'Serial'
            Width = 70
          end
          item
            Caption = 'Issuer'
            Width = 130
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRemove: TButton
        Left = 222
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveClick
      end
      object btnAdd: TButton
        Left = 222
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddClick
      end
    end
  end
  object btnOK: TButton
    Left = 160
    Top = 349
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 241
    Top = 349
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object DlgOpen: TOpenDialog
    Left = 264
    Top = 103
  end
end
