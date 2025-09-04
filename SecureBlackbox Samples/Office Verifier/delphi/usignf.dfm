object FormUsign: TFormUsign
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Signer'
  ClientHeight = 181
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 251
    Top = 147
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object gbSigningCertificates: TGroupBox
    Left = 10
    Top = 8
    Width = 316
    Height = 130
    Caption = 'Known Certificates'
    TabOrder = 1
    object lvSigningCertificates: TListView
      Left = 8
      Top = 24
      Width = 214
      Height = 97
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
      Left = 235
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveClick
    end
    object btnAdd: TButton
      Left = 235
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 2
      OnClick = btnAddClick
    end
  end
  object DlgOpen: TOpenDialog
    Left = 48
    Top = 72
  end
end
