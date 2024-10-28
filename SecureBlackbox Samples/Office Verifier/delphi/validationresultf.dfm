object FormValidationresult: TFormValidationresult
  Left = 566
  Top = 461
  BorderStyle = bsDialog
  Caption = 'Validation results'
  ClientHeight = 311
  ClientWidth = 279
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
    Left = 196
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
  object cbSignatures: TComboBox
    Left = 7
    Top = 32
    Width = 264
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbSignaturesChange
  end
  object pSignatureInfo: TPanel
    Left = 6
    Top = 72
    Width = 264
    Height = 193
    BevelOuter = bvNone
    TabOrder = 2
    object lSignatureType: TLabel
      Left = 5
      Top = 0
      Width = 75
      Height = 13
      Caption = 'Signature type:'
    end
    object lSignatureValidationResult: TLabel
      Left = 5
      Top = 148
      Width = 128
      Height = 13
      Caption = 'Signature Validation Result'
    end
    object lChainValidationResult: TLabel
      Left = 5
      Top = 168
      Width = 109
      Height = 13
      Caption = 'Chain Validation Result'
    end
    object lIsDocumentSigned: TLabel
      Left = 5
      Top = 58
      Width = 91
      Height = 13
      Caption = 'lIsDocumentSigned'
    end
    object lIsSignatureOriginSigned: TLabel
      Left = 5
      Top = 78
      Width = 117
      Height = 13
      Caption = 'lIsSignatureOriginSigned'
    end
    object lIsCorePropertiesSigned: TLabel
      Left = 5
      Top = 98
      Width = 115
      Height = 13
      Caption = 'lIsCorePropertiesSigned'
    end
    object lSignatureTime: TLabel
      Left = 5
      Top = 123
      Width = 70
      Height = 13
      Caption = 'lSignatureTime'
    end
    object editSignatureType: TEdit
      Left = 5
      Top = 16
      Width = 250
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
  end
end
