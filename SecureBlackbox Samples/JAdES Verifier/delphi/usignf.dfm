object FormUsign: TFormUsign
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Signer'
  ClientHeight = 316
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lSignatureIndex: TLabel
    Left = 10
    Top = 18
    Width = 79
    Height = 13
    Caption = 'Signature index:'
  end
  object lIssuerRDN: TLabel
    Left = 10
    Top = 48
    Width = 58
    Height = 13
    Caption = 'Issuer RDN:'
  end
  object lSerialNumber: TLabel
    Left = 10
    Top = 78
    Width = 69
    Height = 13
    Caption = 'Serial number:'
  end
  object lSubjectKeyID: TLabel
    Left = 10
    Top = 108
    Width = 72
    Height = 13
    Caption = 'Subject KeyID:'
  end
  object btnOK: TButton
    Left = 189
    Top = 283
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 270
    Top = 283
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edIssuerRDN: TEdit
    Left = 100
    Top = 45
    Width = 245
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object edSignatureIndex: TEdit
    Left = 100
    Top = 15
    Width = 245
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object gbSigningCertificates: TGroupBox
    Left = 10
    Top = 141
    Width = 335
    Height = 125
    Caption = 'Known Certificates'
    TabOrder = 4
    object lvSigningCertificates: TListView
      Left = 8
      Top = 24
      Width = 240
      Height = 95
      Columns = <
        item
          Caption = 'Serial'
          Width = 100
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
      Left = 253
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveClick
    end
    object btnAdd: TButton
      Left = 253
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 2
      OnClick = btnAddClick
    end
  end
  object edSerialNumber: TEdit
    Left = 100
    Top = 75
    Width = 245
    Height = 21
    ReadOnly = True
    TabOrder = 5
  end
  object edSubjectKeyID: TEdit
    Left = 100
    Top = 105
    Width = 245
    Height = 21
    ReadOnly = True
    TabOrder = 6
  end
  object DlgOpen: TOpenDialog
    Left = 264
    Top = 80
  end
end
