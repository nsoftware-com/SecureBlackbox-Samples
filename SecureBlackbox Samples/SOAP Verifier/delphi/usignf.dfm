object FormUsign: TFormUsign
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Signature Info'
  ClientHeight = 382
  ClientWidth = 325
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
    Left = 16
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
    Top = 33
    Width = 308
    Height = 115
    Caption = 'General'
    TabOrder = 0
    DesignSize = (
      308
      115)
    object lHashAlgorithm: TLabel
      Left = 16
      Top = 62
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lSignatureType: TLabel
      Left = 16
      Top = 29
      Width = 75
      Height = 13
      Caption = 'Signature type:'
    end
    object edSignatureType: TEdit
      Left = 99
      Top = 26
      Width = 196
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
    Top = 154
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
        Width = 214
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
        Left = 225
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 1
        OnClick = btnRemoveClick
      end
      object btnAdd: TButton
        Left = 225
        Top = 25
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
    Top = 105
  end
end
