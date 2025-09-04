object FormReference: TFormReference
  Left = 503
  Top = 425
  BorderStyle = bsDialog
  Caption = 'Reference Options'
  ClientHeight = 611
  ClientWidth = 342
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
  object lbHashAlgorithm: TLabel
    Left = 9
    Top = 103
    Width = 75
    Height = 13
    Caption = 'Hash algorithm:'
  end
  object lbReferenceId: TLabel
    Left = 8
    Top = 15
    Width = 67
    Height = 13
    Caption = 'Reference Id:'
  end
  object lbReferenceType: TLabel
    Left = 8
    Top = 44
    Width = 81
    Height = 13
    Caption = 'Reference Type:'
  end
  object lbReferenceURI: TLabel
    Left = 8
    Top = 74
    Width = 75
    Height = 13
    Caption = 'Reference URI:'
  end
  object cmbHashAlgorithm: TComboBox
    Left = 128
    Top = 99
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
  object edReferenceId: TEdit
    Left = 128
    Top = 12
    Width = 201
    Height = 21
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 174
    Top = 580
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 255
    Top = 580
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object edReferenceType: TEdit
    Left = 128
    Top = 41
    Width = 202
    Height = 21
    TabOrder = 2
  end
  object edReferenceURI: TEdit
    Left = 128
    Top = 71
    Width = 202
    Height = 21
    TabOrder = 5
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 128
    Width = 329
    Height = 345
    TabOrder = 6
    object lbCustomId: TLabel
      Left = 8
      Top = 71
      Width = 53
      Height = 13
      Caption = 'Custom Id:'
    end
    object Label10: TLabel
      Left = 8
      Top = 119
      Width = 313
      Height = 98
      AutoSize = False
      Caption = 
        'Target XML element field used to specify the referenced element.' +
        ' If the reference URI field is empty, then it will be auto-gener' +
        'ated based on the ID of the referenced (target) XML element.'#13#10'To' +
        ' reference document element leave URI and target XML element fie' +
        'lds empty.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object rbTargetXMLElement: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Target XML Element'
      TabOrder = 0
    end
    object edTargetXMLElement: TEdit
      Left = 8
      Top = 36
      Width = 313
      Height = 21
      TabOrder = 1
      OnChange = edTargetXMLElementChange
    end
    object cbAutoGenerateElementId: TCheckBox
      Left = 8
      Top = 96
      Width = 241
      Height = 17
      Caption = 'Auto generate target element Id'
      TabOrder = 2
    end
    object edCustomId: TEdit
      Left = 120
      Top = 68
      Width = 201
      Height = 21
      TabOrder = 3
    end
    object rbTargetData: TRadioButton
      Left = 8
      Top = 216
      Width = 113
      Height = 17
      Caption = 'Target Data'
      TabOrder = 4
    end
    object mmData: TMemo
      Left = 6
      Top = 236
      Width = 315
      Height = 93
      TabOrder = 5
      OnChange = mmDataChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 480
    Width = 329
    Height = 89
    Caption = 'Transforms'
    TabOrder = 7
    DesignSize = (
      329
      89)
    object lbCanonMethod: TLabel
      Left = 8
      Top = 23
      Width = 97
      Height = 26
      AutoSize = False
      Caption = 'Canonicalization transform:'
      WordWrap = True
    end
    object cmbCanonMethod: TComboBox
      Left = 120
      Top = 28
      Width = 201
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Items.Strings = (
        'Canonical'
        'Canonical with comments'
        'Canonical v1.1'
        'Canonical with comments v1.1'
        'Exclusive canonical'
        'Exclusive canonical with comments'
        'Minimal canonical'
        'None')
    end
    object cbUseEnvelopedSignatureTransform: TCheckBox
      Left = 8
      Top = 64
      Width = 305
      Height = 17
      Caption = 'Use enveloped signature transform'
      TabOrder = 1
    end
  end
end
