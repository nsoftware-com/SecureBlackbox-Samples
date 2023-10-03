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
unit dcauthf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ShellApi, SBxCore, SBxTypes,
  SBxPDFSigner, SBxHTTPServer;

type
  TFormDcauth = class(TForm)
    Label10: TLabel;
    Label1: TLabel;
    edPort: TEdit;
    Label2: TLabel;
    mmLog: TMemo;
    bbStart: TButton;
    bbStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxHTTPServer;
    FSigner: TsbxPDFSigner;

    FLogMsg: string;

    procedure DoGetRequest(Sender: TObject; ConnectionID: Int64;
      const URI: String; var Handled: Boolean);
    procedure DoPostRequest(Sender: TObject; ConnectionID: Int64;
      const URI: String; var Handled: Boolean);

    procedure StartSigning(ConnectionID: Int64; const Author, Reason, InputFile: string);
    procedure FinishSigning(ConnectionID: Int64; const Reply: string);
    procedure Log;
    function CopyIndexFile: boolean;
  public
    { Public declarations }
  end;

function Base64Encode(const Value: string): string;

var
  FormDcauth: TFormDcauth;

implementation

{$IF CompilerVersion >= 22.0}
uses System.NetEncoding;
{$ENDIF}

{$R *.dfm}

function ReadAllFile(const FileName: string): string;
var
  FS: TFileStream;
  Buf: RawByteString;
begin
  FS := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Buf, FS.Size);
    FS.ReadBuffer(Buf[1], Length(Buf));

    Result := UTF8Decode(Buf);
  finally
    FreeAndNil(FS);
  end;
end;

function TFormDcauth.CopyIndexFile: boolean;
begin
  Result := true;

  if FileExists('..\index.html') then
    CopyFile('..\index.html', PChar(ExtractFilePath(Application.ExeName) + '\index.html'), false)
  else if FileExists('..\..\index.html') then
    CopyFile('..\..\index.html', PChar(ExtractFilePath(Application.ExeName) + '\index.html'), false)
  else
    Result := false;
end;

procedure TFormDcauth.bbStartClick(Sender: TObject);
begin
  if not CopyIndexFile then
  begin
    ShowMessage('Demo index.html file not found!'#13#10 +
      'Please find the file and put behind example executable.');
    Exit;
  end;

  try
    FServer.Port := StrToInt(edPort.Text);
  except
    ShowMessage('Wrong port value!');
    Exit;
  end;

  if not FServer.Active then
  begin
    FServer.Start;

    bbStart.Enabled := false;
    bbStop.Enabled := true;

    ShellExecute(0, 'open', PChar('http://localhost:' + edPort.Text), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TFormDcauth.bbStopClick(Sender: TObject);
begin
  if FServer.Active then
  begin
    FServer.Stop;

    bbStart.Enabled := true;
    bbStop.Enabled := false;
  end;
end;

procedure TFormDcauth.Log;
begin
  mmLog.Lines.Add(FLogMsg + #13#10);
end;

procedure TFormDcauth.StartSigning(ConnectionID: Int64; const Author, Reason, InputFile: string);
var
  State, HTMLPage: string;
begin
  try
    HTMLPage := ReadAllFile('index.html');
  except
    FServer.SetResponseStatus(ConnectionID, 404);
    FServer.SetResponseString(ConnectionID, '404: index.html not found', 'text/plain');
    Exit;
  end;

  FSigner.InputFile := InputFile;
  FSigner.OutputFile := InputFile + '.presigned';

  // these values should be the same in DC signing application
  FSigner.ExternalCrypto.KeyID := 'key_id';
  FSigner.ExternalCrypto.KeySecret := 'key_secret';

  FSigner.ExternalCrypto.PublicKeyAlgorithm := 'rsaEncryption';
  FSigner.ExternalCrypto.HashAlgorithm := 'SHA256';
  FSigner.ExternalCrypto.Data := InputFile;

  FSigner.Signature.AuthorName := Author;
  FSigner.Signature.Reason := Reason;
  FSigner.Signature.AutoText := true;
  FSigner.Signature.SignerCaption := 'SecureBlackbox Demo Certificate';
  FSigner.Signature.AlgorithmCaption := 'RSA with SHA-256';

  State := FSigner.SignAsyncBegin;
  State := Base64Encode(State);

  HTMLPage := StringReplace(HTMLPage, '${style}', '', [rfReplaceAll, rfIgnoreCase]);
  HTMLPage := StringReplace(HTMLPage, '${data}', State, [rfReplaceAll, rfIgnoreCase]);

  FServer.SetResponseString(ConnectionID, HTMLPage, 'text/html');
end;

procedure TFormDcauth.FinishSigning(ConnectionID: Int64; const Reply: string);
var
  Data: string;
begin
  Data := FSigner.ExtractAsyncData(Reply);

  FSigner.InputFile := Data + '.presigned';
  FSigner.OutputFile := Data + '.signed.pdf';

  FSigner.SignAsyncEnd(Reply);
end;

procedure TFormDcauth.DoGetRequest(Sender: TObject; ConnectionID: Int64;
  const URI: String; var Handled: Boolean);
var
  HTMLPage: string;
begin
  Handled := true;

  FLogMsg := '[' + IntToStr(ConnectionID) + '] GET ' + URI;
  TThread.Synchronize(TThread.CurrentThread, Log);

  if (URI = '/') or (URI = '/index.html') then
  begin
    try
      HTMLPage := ReadAllFile('index.html');
    except
      FServer.SetResponseStatus(ConnectionID, 404);
      FServer.SetResponseString(ConnectionID, '404: index.html not found', 'text/plain');
      Exit;
    end;

    HTMLPage := StringReplace(HTMLPage, '${style}', 'display: none;', [rfReplaceAll, rfIgnoreCase]);
    FServer.SetResponseString(ConnectionID, HTMLPage, 'text/html');
  end
  else
    FServer.SetResponseStatus(ConnectionID, 404);
end;

procedure TFormDcauth.DoPostRequest(Sender: TObject; ConnectionID: Int64;
  const URI: String; var Handled: Boolean);
var
  IDStr, Request, Author, Reason, InputFile: string;
begin
  Handled := true;
  IDStr := IntToStr(ConnectionID);

  Request := FServer.GetRequestString(ConnectionID);

  FLogMsg := '[' + IntToStr(ConnectionID) + '] POST ' + URI + #13#10 + Request;
  TThread.Synchronize(TThread.CurrentThread, Log);

  if URI = '/start' then
  begin
    FServer.Config('RequestFilter[' + IDStr + ']=Params[''author'']');
    Author := FServer.GetRequestString(ConnectionID);

    FServer.Config('RequestFilter[' + IDStr + ']=Params[''reason'']');
    Reason := FServer.GetRequestString(ConnectionID);

    FServer.Config('RequestFilter[' + IDStr + ']=Params[''inputFile'']');
    InputFile := FServer.GetRequestString(ConnectionID);

    StartSigning(ConnectionID, Author, Reason, InputFile);
  end
  else if URI = '/result' then
  begin
    FinishSigning(ConnectionID, Request);
  end
  else
    FServer.SetResponseStatus(ConnectionID, 404);
end;

procedure TFormDcauth.FormCreate(Sender: TObject);
begin
  FServer := TsbxHTTPServer.Create(nil);
  FSigner := TsbxPDFSigner.Create(nil);

  FServer.OnGetRequest := DoGetRequest;
  FServer.OnPostRequest := DoPostRequest;
end;

procedure TFormDcauth.FormDestroy(Sender: TObject);
begin
  bbStopClick(Self);

  FreeAndNil(FServer);
  FreeAndNil(FSigner);
end;

{$IF CompilerVersion >= 22.0}
function Base64Encode(const Value: string): string;
var
  Enc: TBase64Encoding;
begin
  Enc := TBase64Encoding.Create;
  try
    Result := Enc.EncodeBytesToString(BytesOf(Value));
  finally
    Enc.Free;
  end;
end;
{$ELSE}
function Base64Encode(const Value: string): string;
begin
  Old Delphi versions does not have built-in Base64 encoding library.
end;
{$ENDIF}

end.

