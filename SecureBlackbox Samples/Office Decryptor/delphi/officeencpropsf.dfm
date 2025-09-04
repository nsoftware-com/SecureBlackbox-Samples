object FormOfficeencprops: TFormOfficeencprops
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Encryption properties'
  ClientHeight = 317
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    387
    317)
  PixelsPerInch = 96
  TextHeight = 13
  object lInfo: TLabel
    Left = 8
    Top = 8
    Width = 359
    Height = 25
    AutoSize = False
    Caption = 
      'The Office document is encrypted. Please provide a decryption pa' +
      'ssword.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object gbEncryptionProps: TGroupBox
    Left = 8
    Top = 33
    Width = 371
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 361
    ExplicitHeight = 231
    DesignSize = (
      371
      241)
    object lProvidePassword: TLabel
      Left = 16
      Top = 178
      Width = 160
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Provide password for decryption:'
      ExplicitTop = 168
    end
    object lEncryptionInfo: TLabel
      Left = 16
      Top = 21
      Width = 20
      Height = 13
      Caption = 'Info'
    end
    object lDetails: TLabel
      Left = 32
      Top = 40
      Width = 305
      Height = 122
      AutoSize = False
      Caption = 'Details'
    end
    object editPassword: TEdit
      Left = 16
      Top = 197
      Width = 201
      Height = 21
      Anchors = [akLeft, akBottom]
      PasswordChar = '*'
      TabOrder = 0
      ExplicitTop = 187
    end
  end
  object btnOK: TButton
    Left = 116
    Top = 282
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 197
    Top = 282
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
