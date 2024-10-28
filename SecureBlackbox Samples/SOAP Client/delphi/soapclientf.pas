(*
 * SecureBlackbox 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit soapclientf;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, IniFiles,
  sbxtypes, SBxSoapClient;

type
  TFormSoapclient = class(TForm)
    lbInfo: TLabel;
    gbServiceSettings: TGroupBox;
    gbRequest: TGroupBox;
    lbServiceUrl: TLabel;
    edUrl: TEdit;
    lbSoapAction: TLabel;
    edSoapAction: TEdit;
    btnSend: TButton;
    mmRequest: TMemo;
    lbSoapVersion: TLabel;
    cmbSoapVersion: TComboBox;
    gbResponse: TGroupBox;
    mmResponse: TMemo;
    cmbResponseType: TComboBox;
    procedure btnSendClick(Sender: TObject);
    procedure cmbResponseTypeChange(Sender: TObject);
  private
    FResponseSOAPMessage : string;
    FResponseOperationXML : string;
    FResponseOperationTextContent : string;
    FResponseErrors : string;
  public
  end;

var
  FormSoapclient: TFormSoapclient;

implementation

{$R *.dfm}

procedure TFormSoapclient.btnSendClick(Sender: TObject);
var
  Client: TsbxSoapClient;
begin
  FResponseSOAPMessage := '';
  FResponseOperationXML := '';
  FResponseOperationTextContent := '';
  FResponseErrors := '';
  Client := TsbxSoapClient.Create(nil);
  try
    Client.SOAPAction := edSoapAction.Text;
    if cmbSoapVersion.ItemIndex = 1 then
      Client.SOAPVersion := spv11
    else
    if cmbSoapVersion.ItemIndex = 2 then
      Client.SOAPVersion := spv12
    else
      Client.SOAPVersion := spvAuto;

    Client.InputBytes := TEncoding.UTF8.GetBytes(mmRequest.Text);

    try
      Client.SendMessage(edUrl.Text);

      FResponseSOAPMessage := TEncoding.UTF8.GetString(Client.OutputBytes);
      if Client.SOAPResponseType = srtSOAPMessage then
      begin
        FResponseOperationXML := Client.OperationResponseXML;
        FResponseOperationTextContent := Client.OperationResponseTextContent;
      end;

      mmResponse.Text := FResponseOperationTextContent;
      cmbResponseType.ItemIndex := 2;

      MessageDlg('Message has been sent successfully', mtInformation, [mbOk], 0);
    except
      on E: Exception do
      begin
        FResponseSOAPMessage := TEncoding.UTF8.GetString(Client.OutputBytes);
        if Client.StatusCode <> 0 then
          FResponseErrors := Format('HTTP Status code: %d'#13#10, [Client.StatusCode]);

        if Length(Client.ReasonPhrase) > 0 then
          FResponseErrors := FResponseErrors + Format('Reason: %s'#13#10, [Client.ReasonPhrase]);

        if Client.SOAPResponseType = srtSOAPMessage then
        begin
          if Length(Client.FaultActor) > 0 then
            FResponseErrors := FResponseErrors + Format('Fault actor: %s'#13#10, [Client.FaultActor]);

          if Length(Client.FaultCode) > 0 then
            FResponseErrors := FResponseErrors + Format('Fault code: %s'#13#10, [Client.FaultCode]);

          if Length(Client.FaultString) > 0 then
            FResponseErrors := FResponseErrors + Format('Fault string: %s'#13#10, [Client.FaultString]);

          if Length(Client.FaultDetail) > 0 then
            FResponseErrors := FResponseErrors + Format('Fault detail: %s'#13#10, [Client.FaultDetail]);
        end;

        mmResponse.Text := FResponseErrors;
        cmbResponseType.ItemIndex := 3;

        MessageDlg('Failed to send the message:'#13#10 + E.Message, mtError, [mbOk], 0);
      end;
    end;
  finally
    FreeAndNil(Client);
  end;
end;

procedure TFormSoapclient.cmbResponseTypeChange(Sender: TObject);
begin
  if cmbResponseType.ItemIndex = 0 then
    mmResponse.Text := FResponseSOAPMessage
  else
  if cmbResponseType.ItemIndex = 1 then
    mmResponse.Text := FResponseOperationXML
  else
  if cmbResponseType.ItemIndex = 2 then
    mmResponse.Text := FResponseOperationTextContent
  else
  if cmbResponseType.ItemIndex = 3 then
    mmResponse.Text := FResponseErrors;
end;

end.


