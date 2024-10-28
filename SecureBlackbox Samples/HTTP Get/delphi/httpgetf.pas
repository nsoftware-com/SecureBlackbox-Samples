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
unit httpgetf;


{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  SBxTypes, SBxCore, SBxHTTPClient;

type
  TFormHttpget = class(TForm)
    btnGo: TButton;
    MMLog: TMemo;
    btnHead: TButton;
    dlgSave: TSaveDialog;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EdHost: TEdit;
    CbProtocol: TComboBox;
    EdPort: TEdit;
    EdPath: TEdit;
    EdFileName: TEdit;
    BtSel: TButton;
    procedure btnGoClick(Sender: TObject);
    procedure HttpClientCertificateValidate(Sender: TObject; const ServerHost : string; const ServerIP : string; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure HttpClientDocumentStart(Sender: TObject);
    procedure HttpClientDocumentFinish(Sender: TObject);
    procedure HttpClientReceivingHeaders(Sender: TObject);
    procedure HttpClientRedirection(Sender: TObject; const OldURL : string; var NewURL: String; var AllowRedirection: Boolean);
    procedure HttpClientPreparedHeaders(Sender: TObject);
    procedure btnHeadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtSelClick(Sender: TObject);
  private
    { Private declarations }
    HttpClient: TsbxHTTPClient;
  public
    FLogFile: TextFile;
  end;

var
  FormHttpget: TFormHttpget;

implementation

{$R *.DFM}

procedure TFormHttpget.btnGoClick(Sender: TObject);
begin
  mmLog.Lines.Clear;
  if EdPath.Text = '' then
    EdPath.Text := '/';
  //metacode - disable 'Go' button
  btnGo.Enabled := False;

  //metacode - fetch the page
  try
    try
      HttpClient.OutputFile := EdFileName.Text;

      HttpClient.Get(CbProtocol.Text + '://' + EdHost.Text + ':' + EdPort.Text + EdPath.Text);

      MMLog.Lines.Add(HttpClient.OutputString);
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

procedure TFormHttpget.HttpClientCertificateValidate(Sender: TObject; const ServerHost : string; const ServerIP : string; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormHttpget.FormCreate(Sender: TObject);
begin
  CbProtocol.ItemIndex := 0;
  CbProtocol.ItemIndex := 1;
  EdHost.Text := 'www.microsoft.com';
  EdPort.Text := '443';

  HttpClient := TsbxHTTPClient.Create(nil);

  HttpClient.OnTLSCertValidate := HttpClientCertificateValidate;
  HttpClient.OnDocumentBegin := HttpClientDocumentStart;
  HttpClient.OnDocumentEnd := HttpClientDocumentFinish;
  HttpClient.OnHeadersPrepared := HttpClientPreparedHeaders;
  HttpClient.OnHeadersReceived := HttpClientReceivingHeaders;
  HttpClient.OnRedirection := HttpClientRedirection;
end;

procedure TFormHttpget.FormDestroy(Sender: TObject);
begin
  FreeAndNil(HttpClient);
end;

procedure TFormHttpget.HttpClientDocumentStart(Sender: TObject);
begin
  MMLog.Lines.Add('--- Document started ---');
  Application.ProcessMessages;
end;

procedure TFormHttpget.HttpClientDocumentFinish(Sender: TObject);
begin
  MMLog.Lines.Add('--- Document finished ---');
  Application.ProcessMessages;
end;

procedure TFormHttpget.HttpClientReceivingHeaders(Sender: TObject);
var
  i : integer;
begin
  MMLog.Lines.Add('Headers received: ');
  for i := 0 to HttpClient.ResponseHeaders.Count - 1 do
    MMLog.Lines.Add(TsbxStringNameValuePair(HttpClient.ResponseHeaders.Item[i]).Name + ': ' + TsbxStringNameValuePair(HttpClient.ResponseHeaders.Item[i]).Value);
  MMLog.Lines.Add('');
  Application.ProcessMessages;
end;

procedure TFormHttpget.HttpClientRedirection(Sender: TObject; const OldURL : string; var NewURL: String; var AllowRedirection: Boolean);
begin
  MMLog.Lines.Add('Request redirected to ' + NewURL);
  AllowRedirection := true;
  Application.ProcessMessages;
end;

procedure TFormHttpget.HttpClientPreparedHeaders(Sender: TObject);
var
  i : integer;
begin
  MMLog.Lines.Add('Headers sent: ');
  for i := 0 to HttpClient.RequestHeaders.Count - 1 do
    MMLog.Lines.Add(TsbxStringNameValuePair(HttpClient.RequestHeaders.Item[i]).Name + ': ' + TsbxStringNameValuePair(HttpClient.RequestHeaders.Item[i]).Value);
  MMLog.Lines.Add('');
  Application.ProcessMessages;
end;

procedure TFormHttpget.btnHeadClick(Sender: TObject);
begin
  mmLog.Lines.Clear;
  if EdPath.Text = '' then
    EdPath.Text := '/';
  //metacode - disable 'Go' button
  btnGo.Enabled := False;
  //metacode - fetch the page
  try
    try
      HttpClient.Head(CbProtocol.Text + '://' + EdHost.Text + ':' + EdPort.Text + EdPath.Text);

      MMLog.Lines.Add(HttpClient.OutputString);
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

procedure TFormHttpget.BtSelClick(Sender: TObject);
begin
  if dlgSave.Execute then
    EdFileName.Text := dlgSave.FileName;
end;

end.
