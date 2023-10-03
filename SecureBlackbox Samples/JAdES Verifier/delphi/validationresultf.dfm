object FormValidationresult: TFormValidationresult
  Left = 566
  Top = 461
  Caption = 'Validation results'
  ClientHeight = 222
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    306
    222)
  PixelsPerInch = 96
  TextHeight = 13
  object lSignatures: TLabel
    Left = 8
    Top = 13
    Width = 288
    Height = 13
    Caption = 'The file contains the following digital JWS/JAdES signatures:'
  end
  object btnClose: TButton
    Left = 223
    Top = 189
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
  object cbSignatures: TComboBox
    Left = 8
    Top = 32
    Width = 288
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbSignaturesChange
  end
  object pSignatureInfo: TPanel
    Left = 8
    Top = 59
    Width = 290
    Height = 118
    BevelOuter = bvNone
    TabOrder = 2
    object lTimestamp: TLabel
      Left = 6
      Top = 37
      Width = 55
      Height = 13
      Caption = 'Timestamp:'
    end
    object lSignatureValidationResult: TLabel
      Left = 6
      Top = 64
      Width = 128
      Height = 13
      Caption = 'Signature Validation Result'
    end
    object lChainValidationResult: TLabel
      Left = 6
      Top = 91
      Width = 109
      Height = 13
      Caption = 'Chain Validation Result'
    end
    object lTimestamped: TLabel
      Left = 6
      Top = 10
      Width = 67
      Height = 13
      Caption = 'Timestamped:'
    end
  end
end
