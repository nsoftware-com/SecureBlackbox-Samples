object FormReference: TFormReference
  Left = 503
  Top = 425
  BorderStyle = bsDialog
  Caption = 'Reference options'
  ClientHeight = 391
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    299
    391)
  PixelsPerInch = 96
  TextHeight = 13
  object lbTargetXMLElement: TLabel
    Left = 8
    Top = 109
    Width = 99
    Height = 13
    Caption = 'Target XML Element:'
  end
  object lbHashAlgorithm: TLabel
    Left = 9
    Top = 75
    Width = 75
    Height = 13
    Caption = 'Hash algorithm:'
  end
  object lbID: TLabel
    Left = 8
    Top = 16
    Width = 15
    Height = 13
    Caption = 'ID:'
  end
  object lbRefType: TLabel
    Left = 8
    Top = 42
    Width = 28
    Height = 13
    Caption = 'Type:'
  end
  object lbTargetData: TLabel
    Left = 9
    Top = 224
    Width = 62
    Height = 13
    Caption = 'Target Data:'
  end
  object lbCanonMethod: TLabel
    Left = 8
    Top = 183
    Width = 77
    Height = 26
    Caption = 'Canonicalization'#13#10'method:'
  end
  object edTargetXMLElement: TEdit
    Left = 8
    Top = 128
    Width = 282
    Height = 21
    TabOrder = 2
  end
  object cmbHashAlgorithm: TComboBox
    Left = 88
    Top = 71
    Width = 202
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      'SHA1'
      'MD5'
      'SHA224'
      'SHA256'
      'SHA384'
      'SHA512'
      'RIPEMD160')
  end
  object mmData: TMemo
    Left = 6
    Top = 245
    Width = 284
    Height = 93
    TabOrder = 4
  end
  object edID: TEdit
    Left = 48
    Top = 12
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 134
    Top = 356
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 215
    Top = 356
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object edRefType: TEdit
    Left = 48
    Top = 39
    Width = 242
    Height = 21
    TabOrder = 3
  end
  object cbAutoGenerateElementId: TCheckBox
    Left = 8
    Top = 162
    Width = 241
    Height = 17
    Caption = 'Auto generate target element Id'
    TabOrder = 7
  end
  object cmbCanonMethod: TComboBox
    Left = 92
    Top = 185
    Width = 199
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    Items.Strings = (
      'Canonical'
      'Canonical with comments'
      'Canonical v1.1'
      'Canonical with comments v1.1'
      'Exclusive canonical'
      'Exclusive canonical with comments'
      'Minimal canonical'
      'None')
    ExplicitWidth = 198
  end
end
