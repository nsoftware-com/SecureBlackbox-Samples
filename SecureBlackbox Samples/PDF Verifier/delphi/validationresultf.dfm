object FormValidationresult: TFormValidationresult
  Left = 566
  Top = 461
  BorderStyle = bsDialog
  Caption = 'Validation results'
  ClientHeight = 331
  ClientWidth = 280
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
    Left = 195
    Top = 300
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
    Height = 209
    BevelOuter = bvNone
    TabOrder = 2
    object lAuthorName: TLabel
      Left = 5
      Top = 0
      Width = 73
      Height = 13
      Caption = 'Author'#39's name:'
    end
    object lReason: TLabel
      Left = 5
      Top = 48
      Width = 93
      Height = 13
      Caption = 'Reason for signing:'
    end
    object lTimestamp: TLabel
      Left = 5
      Top = 96
      Width = 55
      Height = 13
      Caption = 'Timestamp:'
    end
    object lSignatureValidationResult: TLabel
      Left = 5
      Top = 120
      Width = 128
      Height = 13
      Caption = 'Signature Validation Result'
    end
    object lChainValidationResult: TLabel
      Left = 5
      Top = 143
      Width = 109
      Height = 13
      Caption = 'Chain Validation Result'
    end
    object editAuthorName: TEdit
      Left = 5
      Top = 16
      Width = 250
      Height = 21
      TabOrder = 0
    end
    object editReason: TEdit
      Left = 5
      Top = 64
      Width = 250
      Height = 21
      TabOrder = 1
    end
    object btnExtractSigned: TButton
      Left = 108
      Top = 176
      Width = 147
      Height = 25
      Caption = 'Extract signed version'
      TabOrder = 2
      OnClick = btnExtractSignedClick
    end
  end
  object SaveDialog: TSaveDialog
    Filter = 'PDF documents (*.pdf)|*.pdf|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 34
    Top = 264
  end
end
