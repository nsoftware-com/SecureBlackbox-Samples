(*
 * SecureBlackbox 2022 Delphi Edition - Sample Project
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
unit httppostf;


{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  SBxTypes, SBxCore, SBxHTTPClient;

type
  TFormHttppost = class(TForm)
    Label1: TLabel;
    EdURL: TEdit;
    btnGo: TButton;
    MMLog: TMemo;
    EdFileName: TEdit;
    Label5: TLabel;
    BtSel: TButton;
    cbPostAsForm: TCheckBox;
    dlgOpen: TOpenDialog;
    Label10: TLabel;
    procedure btnGoClick(Sender: TObject);
    procedure HttpClientCertificateValidate(Sender: TObject; const ServerHost : string; const ServerIP : string; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure HttpClientDocumentStart(Sender: TObject);
    procedure HttpClientDocumentFinish(Sender: TObject);
    procedure HttpClientReceivingHeaders(Sender: TObject);
    procedure HttpClientRedirection(Sender: TObject; const OldURL : string; var NewURL: String; var AllowRedirection: Boolean);
    procedure HttpClientPreparedHeaders(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtSelClick(Sender: TObject);
  private
    { Private declarations }
    HttpClient: TsbxHTTPClient;
  public
    FLogFile: TextFile;
  end;

var
  FormHttppost: TFormHttppost;

implementation

{$R *.DFM}

procedure TFormHttppost.btnGoClick(Sender: TObject);
begin
  if (Trim(EdURL.Text) <> '') and (Trim(EdFileName.Text) <> '') then
  begin
    mmLog.Lines.Clear;
    btnGo.Enabled := False;

    try
      try
        if cbPostAsForm.Checked then
          HttpClient.PostWebForm(EdURL.Text, 'upload=Upload', 'userfile', EdFileName.Text, '')
        else
          HttpClient.PostFile(EdURL.Text, EdFileName.Text);
      finally
        btnGo.Enabled := True;
      end;
    except
      on E : Exception do
      begin
        MMLog.Lines.Add('Exception: ' + E.Message);
      end;
    end;
  end;
end;

procedure TFormHttppost.HttpClientCertificateValidate(Sender: TObject; const ServerHost : string; const ServerIP : string; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormHttppost.FormCreate(Sender: TObject);
begin
  HttpClient := TsbxHTTPClient.Create(nil);

  HttpClient.OnTLSCertValidate := HttpClientCertificateValidate;
  HttpClient.OnDocumentBegin := HttpClientDocumentStart;
  HttpClient.OnDocumentEnd := HttpClientDocumentFinish;
  HttpClient.OnHeadersPrepared := HttpClientPreparedHeaders;
  HttpClient.OnHeadersReceived := HttpClientReceivingHeaders;
  HttpClient.OnRedirection := HttpClientRedirection;
end;

procedure TFormHttppost.FormDestroy(Sender: TObject);
begin
  FreeAndNil(HttpClient);
end;

procedure TFormHttppost.HttpClientDocumentStart(Sender: TObject);
begin
  MMLog.Lines.Add('--- Document started ---');
  Application.ProcessMessages;
end;

procedure TFormHttppost.HttpClientDocumentFinish(Sender: TObject);
begin
  MMLog.Lines.Add('--- Document finished ---');
  Application.ProcessMessages;
end;

procedure TFormHttppost.HttpClientReceivingHeaders(Sender: TObject);
var
  i : integer;
begin
  MMLog.Lines.Add('Headers received: ');
  for i := 0 to HttpClient.ResponseHeaders.Count - 1 do
    MMLog.Lines.Add(TsbxStringNameValuePair(HttpClient.ResponseHeaders.Item[i]).Name + ': ' + TsbxStringNameValuePair(HttpClient.ResponseHeaders.Item[i]).Value);
  MMLog.Lines.Add('');
  Application.ProcessMessages;
end;

procedure TFormHttppost.HttpClientRedirection(Sender: TObject; const OldURL : string; var NewURL: String; var AllowRedirection: Boolean);
begin
  MMLog.Lines.Add('Request redirected to ' + NewURL);
  AllowRedirection := true;
  Application.ProcessMessages;
end;

procedure TFormHttppost.HttpClientPreparedHeaders(Sender: TObject);
var
  i : integer;
begin
  MMLog.Lines.Add('Headers sent: ');
  for i := 0 to HttpClient.RequestHeaders.Count - 1 do
    MMLog.Lines.Add(TsbxStringNameValuePair(HttpClient.RequestHeaders.Item[i]).Name + ': ' + TsbxStringNameValuePair(HttpClient.RequestHeaders.Item[i]).Value);
  MMLog.Lines.Add('');
  Application.ProcessMessages;
end;

procedure TFormHttppost.BtSelClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    EdFileName.Text := dlgOpen.FileName;
end;

end.






