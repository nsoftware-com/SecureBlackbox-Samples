object FormUsign: TFormUsign
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Signer'
  ClientHeight = 271
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    364
    271)
  PixelsPerInch = 96
  TextHeight = 13
  object lIssuerRDN: TLabel
    Left = 12
    Top = 11
    Width = 58
    Height = 13
    Caption = 'Issuer RDN:'
  end
  object lSerialNumber: TLabel
    Left = 12
    Top = 41
    Width = 69
    Height = 13
    Caption = 'Serial number:'
  end
  object lSubjectKeyID: TLabel
    Left = 12
    Top = 71
    Width = 72
    Height = 13
    Caption = 'Subject KeyID:'
  end
  object btnOK: TButton
    Left = 200
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 197
    ExplicitTop = 254
  end
  object btnCancel: TButton
    Left = 281
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 268
    ExplicitTop = 244
  end
  object edIssuerRDN: TEdit
    Left = 102
    Top = 8
    Width = 254
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 2
    ExplicitWidth = 241
  end
  object gbSigningCertificates: TGroupBox
    Left = 12
    Top = 104
    Width = 344
    Height = 120
    Caption = 'Known Certificates'
    TabOrder = 3
    object lvSigningCertificates: TListView
      Left = 5
      Top = 24
      Width = 245
      Height = 90
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
      Left = 260
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveClick
    end
    object btnAdd: TButton
      Left = 260
      Top = 25
      Width = 75
      Height = 24
      Caption = 'Add'
      TabOrder = 2
      OnClick = btnAddClick
    end
  end
  object edSerialNumber: TEdit
    Left = 102
    Top = 38
    Width = 254
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 4
    ExplicitWidth = 241
  end
  object edSubjectKeyID: TEdit
    Left = 102
    Top = 68
    Width = 254
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 5
    ExplicitWidth = 241
  end
  object DlgOpen: TOpenDialog
    Left = 266
    Top = 43
  end
end
