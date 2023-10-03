object FormValidationresult: TFormValidationresult
  Left = 566
  Top = 461
  Caption = 'Validation results'
  ClientHeight = 408
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
  object lSignatures: TLabel
    Left = 8
    Top = 16
    Width = 263
    Height = 13
    Caption = 'The document contains the following digital signatures:'
  end
  object btnClose: TButton
    Left = 259
    Top = 375
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
  object cbSignatures: TComboBox
    Left = 7
    Top = 32
    Width = 322
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbSignaturesChange
  end
  object pSignatureInfo: TPanel
    Left = 8
    Top = 72
    Width = 323
    Height = 281
    BevelOuter = bvNone
    TabOrder = 2
    object lSignatureValidationResult: TLabel
      Left = 10
      Top = 232
      Width = 128
      Height = 13
      Caption = 'Signature Validation Result'
    end
    object lSignatureName: TLabel
      Left = 10
      Top = 35
      Width = 75
      Height = 13
      Caption = 'Signature type:'
    end
    object lIssuerRDN: TLabel
      Left = 10
      Top = 73
      Width = 58
      Height = 13
      Caption = 'Issuer RDN:'
    end
    object lSerialNumber: TLabel
      Left = 10
      Top = 103
      Width = 69
      Height = 13
      Caption = 'Serial number:'
    end
    object lSubjectKeyID: TLabel
      Left = 10
      Top = 133
      Width = 72
      Height = 13
      Caption = 'Subject KeyID:'
    end
    object lTimestamp: TLabel
      Left = 10
      Top = 164
      Width = 55
      Height = 13
      Caption = 'Timestamp:'
    end
    object lSignedFiles: TLabel
      Left = 10
      Top = 194
      Width = 60
      Height = 13
      Caption = 'Signed Files:'
    end
    object Label1: TLabel
      Left = 10
      Top = 8
      Width = 29
      Height = 13
      Caption = 'Level:'
    end
    object lChainValidationResult: TLabel
      Left = 10
      Top = 264
      Width = 109
      Height = 13
      Caption = 'Chain Validation Result'
    end
    object edIssuerRDN: TEdit
      Left = 95
      Top = 70
      Width = 228
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 0
    end
    object edSignatureType: TEdit
      Left = 95
      Top = 32
      Width = 228
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 1
    end
    object edSerialNumber: TEdit
      Left = 95
      Top = 100
      Width = 228
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 2
    end
    object edSubjectKeyID: TEdit
      Left = 95
      Top = 127
      Width = 228
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 3
    end
    object edTimestamp: TEdit
      Left = 95
      Top = 161
      Width = 228
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 4
    end
    object edSignedFiles: TEdit
      Left = 95
      Top = 191
      Width = 228
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 5
    end
    object edLevel: TEdit
      Left = 95
      Top = 5
      Width = 228
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 6
    end
  end
end
