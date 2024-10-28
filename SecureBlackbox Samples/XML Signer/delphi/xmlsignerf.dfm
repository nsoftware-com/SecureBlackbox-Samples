object FormXmlsigner: TFormXmlsigner
  Left = 227
  Top = 151
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'XML Signer demo'
  ClientHeight = 305
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbXMLFile: TLabel
    Left = 8
    Top = 51
    Width = 47
    Height = 13
    Caption = 'Input file:'
  end
  object sbBrowseXMLFile: TSpeedButton
    Left = 343
    Top = 45
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbBrowseXMLFileClick
  end
  object Label1: TLabel
    Left = 8
    Top = 81
    Width = 55
    Height = 13
    Caption = 'Output file:'
  end
  object sbOutputFile: TSpeedButton
    Left = 343
    Top = 75
    Width = 75
    Height = 25
    Caption = 'Browse ...'
    OnClick = sbOutputFileClick
  end
  object Label10: TLabel
    Left = 8
    Top = 15
    Width = 640
    Height = 13
    Caption = 
      'This sample shows how to create XML signatures. Please select an' +
      ' input and output files, tune up the signing options, and click ' +
      #39'Sign'#39'. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object edXMLFile: TEdit
    Left = 69
    Top = 47
    Width = 268
    Height = 21
    TabOrder = 0
  end
  object edOutputFile: TEdit
    Left = 69
    Top = 77
    Width = 268
    Height = 21
    TabOrder = 1
  end
  object btnReferences: TButton
    Left = 8
    Top = 270
    Width = 75
    Height = 25
    Caption = 'References'
    TabOrder = 3
    OnClick = btnReferencesClick
  end
  object btnSign: TButton
    Left = 606
    Top = 270
    Width = 75
    Height = 25
    Caption = 'Sign'
    TabOrder = 4
    OnClick = btnSignClick
  end
  object gbSigningOptions: TGroupBox
    Left = 8
    Top = 106
    Width = 673
    Height = 151
    Caption = 'Signing options  '
    TabOrder = 2
    DesignSize = (
      673
      151)
    object lbKeyName: TLabel
      Left = 46
      Top = 114
      Width = 52
      Height = 13
      Caption = 'Key Name:'
    end
    object Label5: TLabel
      Left = 48
      Top = 50
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label3: TLabel
      Left = 10
      Top = 23
      Width = 89
      Height = 13
      Caption = 'Signing certificate:'
    end
    object sbSignCertFile: TSpeedButton
      Left = 335
      Top = 17
      Width = 75
      Height = 25
      Caption = 'Browse ...'
      OnClick = sbSignCertFileClick
    end
    object lbHashAlgorithm: TLabel
      Left = 424
      Top = 22
      Width = 75
      Height = 13
      Caption = 'Hash algorithm:'
    end
    object lbCanonMethod: TLabel
      Left = 418
      Top = 50
      Width = 81
      Height = 13
      Caption = 'Canonicalization:'
    end
    object lbSignatureNode: TLabel
      Left = 422
      Top = 115
      Width = 77
      Height = 13
      Caption = 'Signature node:'
    end
    object edKeyName: TEdit
      Left = 104
      Top = 111
      Width = 225
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
    end
    object cbIncludeKey: TCheckBox
      Left = 10
      Top = 88
      Width = 209
      Height = 17
      Caption = 'Include Key (public part)'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object edCertPassword: TEdit
      Left = 104
      Top = 46
      Width = 185
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object edSigningCertificate: TEdit
      Left = 104
      Top = 19
      Width = 225
      Height = 21
      TabOrder = 0
    end
    object cmbCanonMethod: TComboBox
      Left = 505
      Top = 46
      Width = 160
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'Canonical'
        'Canonical with comments'
        'Canonical v1.1'
        'Canonical with comments v1.1'
        'Exclusive canonical'
        'Exclusive canonical with comments'
        'Minimal canonical')
    end
    object cmbHashAlgorithm: TComboBox
      Left = 505
      Top = 19
      Width = 160
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Items.Strings = (
        'SHA1'
        'MD5'
        'SHA256'
        'SHA384'
        'SHA512'
        'RIPEMD160')
    end
    object cbDetached: TCheckBox
      Left = 394
      Top = 88
      Width = 159
      Height = 17
      Caption = 'Create detached signature'
      TabOrder = 6
    end
    object edSignatureNode: TEdit
      Left = 505
      Top = 111
      Width = 160
      Height = 21
      TabOrder = 7
    end
  end
  object dlgOpenXML: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 560
    Top = 45
  end
  object dlgSaveXML: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'XML files|*.xml|All files|*.*'
    Left = 632
    Top = 45
  end
  object DlgOpen: TOpenDialog
    Left = 488
    Top = 45
  end
end


