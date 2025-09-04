object FormSoapclient: TFormSoapclient
  Left = 506
  Top = 277
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Soap Client'
  ClientHeight = 765
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    560
    765)
  PixelsPerInch = 96
  TextHeight = 13
  object lbInfo: TLabel
    Left = 8
    Top = 8
    Width = 544
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This sample shows how to send a SOAP message using the SoapClien' +
      't component.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 497
  end
  object gbServiceSettings: TGroupBox
    Left = 8
    Top = 27
    Width = 544
    Height = 126
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Service Settings'
    TabOrder = 0
    DesignSize = (
      544
      126)
    object lbServiceUrl: TLabel
      Left = 14
      Top = 25
      Width = 61
      Height = 13
      Caption = 'Service URL:'
    end
    object lbSoapAction: TLabel
      Left = 14
      Top = 60
      Width = 64
      Height = 13
      Caption = 'SOAP Action:'
    end
    object lbSoapVersion: TLabel
      Left = 14
      Top = 92
      Width = 69
      Height = 13
      Caption = 'SOAP Version:'
    end
    object edUrl: TEdit
      Left = 105
      Top = 22
      Width = 428
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'https://graphical.weather.gov/xml/SOAP_server/ndfdXMLserver.php'
    end
    object edSoapAction: TEdit
      Left = 105
      Top = 57
      Width = 428
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 
        'https://graphical.weather.gov/xml/DWMLgen/wsdl/ndfdXML.wsdl#LatL' +
        'onListZipCode'
    end
    object cmbSoapVersion: TComboBox
      Left = 105
      Top = 89
      Width = 145
      Height = 21
      ItemIndex = 1
      TabOrder = 2
      Text = '1.1'
      Items.Strings = (
        'Auto'
        '1.1'
        '1.2')
    end
  end
  object gbRequest: TGroupBox
    Left = 8
    Top = 164
    Width = 544
    Height = 321
    Anchors = [akLeft, akTop, akRight]
    Caption = 'SOAP Message / XML Operation request'
    TabOrder = 1
    DesignSize = (
      544
      321)
    object mmRequest: TMemo
      Left = 14
      Top = 28
      Width = 519
      Height = 282
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        '<w:LatLonListZipCodeRequest '
        
          'xmlns:w="https://graphical.weather.gov/xml/DWMLgen/wsdl/ndfdXML.' +
          'wsdl">'
        '<w:zipCodeList>'
        '27517'
        '</w:zipCodeList>'
        '</w:LatLonListZipCodeRequest>')
      TabOrder = 0
    end
  end
  object btnSend: TButton
    Left = 8
    Top = 492
    Width = 544
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Send Message'
    TabOrder = 2
    OnClick = btnSendClick
  end
  object gbResponse: TGroupBox
    Left = 8
    Top = 522
    Width = 544
    Height = 235
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Response'
    TabOrder = 3
    DesignSize = (
      544
      235)
    object mmResponse: TMemo
      Left = 14
      Top = 64
      Width = 519
      Height = 160
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
    end
    object cmbResponseType: TComboBox
      Left = 14
      Top = 24
      Width = 219
      Height = 21
      ItemIndex = 0
      TabOrder = 1
      Text = 'SOAP Message'
      OnChange = cmbResponseTypeChange
      Items.Strings = (
        'SOAP Message'
        'Operation XML'
        'Operation Text Content'
        'Faults')
    end
  end
end


