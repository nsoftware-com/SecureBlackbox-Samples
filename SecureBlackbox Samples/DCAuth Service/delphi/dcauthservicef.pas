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
﻿unit dcauthservicef;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage, System.NetEncoding, ShellAPI,
  sbxDCAuth, sbxHttpServer, sbxPublicKeyCrypto, sbxCAdESSigner, sbxCertificateManager, sbxCryptoKeyManager, sbxTypes;

const
  WM_LOG = WM_USER + 1;

  PKCS1Format = '?format=pkcs1';
  PKCS7Format = '?format=pkcs7';

type
  TFormDcauthservice = class(TForm)
    Label1: TLabel;
    editPort: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    editKeyID: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    editKeySecret: TEdit;
    btnStart: TButton;
    Label6: TLabel;
    editCertFile: TEdit;
    Label7: TLabel;
    editCertPass: TEdit;
    Label8: TLabel;
    btnStop: TButton;
    memoLog: TMemo;
    Label9: TLabel;
    editSigningEndpoint: TEdit;
    lServiceInfo: TLabel;
    btnLaunchWebApp: TButton;
    Label11: TLabel;
    iInfo: TImage;
    cbEnableJSMode: TCheckBox;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure lServiceInfoClick(Sender: TObject);
    procedure btnLaunchWebAppClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    // The web server that listens to relayed async requests from the browser.
    FWebServer : TsbxHTTPServer;

    // The signing endpoint on the web service - e.g. /sign, but can be anything.
    FSignEndpoint : string;

    // The DCAuth authentication credentials: KeyID and KeySecret.
    // These two should match the ones used by the pre-signing party, such as PDF signer.
    FKeyId : string;
    FKeySecret : string;

    // The path to the signing certificate. This must have an associated private key,
    // so normally it will be in .pfx format. Sometimes PEM is also used, but
    // make sure it does contain the private key.
    FCertFile : string;

    mgr : TsbxCertificateManager;

    // The password to decrypt the certificate or key.
    FCertPassword : string;

    // Logging methods
    procedure Log(const S : string);
    procedure LogAsync(const S : string);
    procedure LogCapture(var Msg : TMessage); message WM_LOG;

    // Outputs welcome/information page. It is only used for debug/testing purposes,
    // a live signing service does not need it.
    procedure WelcomePage(ConnectionID : Int64);

    // Outputs the content produced by the signing endpoint. This is not really
    // an HTML page; it returns an XML document containing the async response.
    procedure SignPage(ConnectionID : Int64);

    // Outputs the content produced by the java script signing endpoint. Just as SignPage(),
    // this is not an HTML page; it returns a hexadecimal-encoded signature value
    // to be placed in the async response.
    procedure SignPageJSMode(ConnectionID : Int64; IsPKCS7 : boolean);

    // Outputs the error page
    procedure ErrorPage(ConnectionID : Int64; const ErrorMsg : string);

    // Implements the logic of the DCAuth request processing. Takes a DCAuth request
    // on input, and produces a DCAuth response that matches it.
    function SignAsyncRequest(const Request : string): string;

    // Takes a hash on input and produces a java script PKCS1 signature over it.
    function SignPKCS1Hash(const Hash : TBytes): TBytes;

    // Takes a hash on input and produces a java script PKCS7 signature over it.
    function SignPKCS7Hash(const Hash : TBytes): TBytes;

    // GET and POST request handlers for the web server component.
    procedure HandleServerGetRequest(Sender: TObject; ConnectionID: Int64; const URI: String;
      var Handled: Boolean);
    procedure HandleServerPostRequest(Sender: TObject; ConnectionID: Int64; const URI: String;
      var Handled: Boolean);
  public
    { Public declarations }
  end;

var
  FormDcauthservice: TFormDcauthservice;

implementation

{$R *.dfm}

const
  CrLf = #13#10;

function HexDecodeChar(C : char): byte;
begin
  if (C >= '0') and (C <= '9') then
    Result := Ord(C) - Ord('0')
  else if (C >= 'a') and (C <= 'f') then
    Result := 10 + Ord(C) - Ord('a')
  else if (C >= 'A') and (C <= 'F') then
    Result := 10 + Ord(C) - Ord('A')
  else
    Result := 255;
end;

function HexDecode(const S : string): TBytes;
var
  I : integer;
  B1, B2 : byte;
begin
  if Length(S) mod 2 <> 0 then
    raise Exception.Create('Invalid hash value: length not even');

  SetLength(Result, Length(S) div 2);

  I := 0;
  while I < Length(Result) do
  begin
    B1 := HexDecodeChar(S[I * 2 + 1]);
    B2 := HexDecodeChar(S[I * 2 + 2]);

    if (B1 = 255) or (B2 = 255) then
      raise Exception.Create('Invalid hash value: non-hexadecimal characters');

    Result[I] := (B1 shl 4) or B2;

    Inc(I);
  end;
end;

function HexEncode(const V : TBytes): string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to Length(V) - 1 do
    Result := Result + IntToHex(V[I], 2);
end;

procedure TFormDcauthservice.btnLaunchWebAppClick(Sender: TObject);
var
  URL : string;
begin
  if not FWebServer.Active then
    if MessageDlg('The DCAuth signing service is not active. The web app will be unable to request a signature from it. Are you sure you want to launch it? (hint: start the service with the Start button at the left first)',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  URL := 'http://127.0.0.1:16080/';
  MessageDlg('The demo will now try to open a web application at its known default location of ' + URL +
    '. Please make sure the web application (the second part of the demo) is running. ' +
    'If you changed any settings in the web app, this button may fail to work, and you ' +
    'might need to open the application manually by typing the correct URL in your browser.',
    mtInformation, [mbOk], 0);

  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL) ;
end;

procedure TFormDcauthservice.btnStartClick(Sender: TObject);
begin
  if FWebServer.Active then
    ShowMessage('Please stop the server first!')
  else
  begin
    { Saving DCAuth protocol parameters }

    // The signing endpoint (relative to the local host, e.g. /sign)
    // This endpoint accepts DCAuth async requests and produces DCAuth
    // async responses. It is accessed by the DCAuth web app in its default
    // mode.
    FSignEndpoint := editSigningEndpoint.Text;

    // Authentication credentials
    FKeyId := editKeyID.Text;
    FKeySecret := editKeySecret.Text;

    // Signing certificate parameters
    if not FileExists(editCertFile.Text) then
    begin
      ShowMessage('Please make sure you provide a signing certificate that includes its private key. The service will be unable to sign async requests otherwise.');
      Exit;
    end;

    FCertFile := editCertFile.Text;
    FCertPassword := editCertPass.Text;

    { Preparing the web server }

    // Port to listen on
    FWebServer.Port := StrToIntDef(editPort.Text, 8080);

    // HTTP request handlers: we want to intercept GETs and POSTs
    FWebServer.OnGetRequest := HandleServerGetRequest;
    FWebServer.OnPostRequest := HandleServerPostRequest;

    // No need for keep-alive connections
    FWebServer.AllowKeepAlive := false;

    // Starting the server
    FWebServer.Start;

    Log('The web endpoint has started');
  end;
end;

procedure TFormDcauthservice.btnStopClick(Sender: TObject);
begin
  if not FWebServer.Active then
    ShowMessage('The server is not running')
  else
    FWebServer.Stop;

  Log('The web endpoint has stopped');
end;

procedure TFormDcauthservice.FormCreate(Sender: TObject);
begin
  FWebServer := TsbxHTTPServer.Create(nil);
  mgr := TsbxCertificateManager.Create(nil);

  iInfo.Hint := 'The web app is the second part of the demo and should be run separately, on this or different system.' + #13#10 + #13#10 +
    'To evaluate the demo, please do the following:' + #13#10 + #13#10 +
    '1) Run this application, set up the parameters of the service (if needed), and start the service.' + #13#10 +
    '2) Run the second part of the demo (the web application) on this or different system.' + #13#10 +
    '3) Navigate to the web application in your local browser. In default configuration, and when running both' + #13#10 +
    '    parts of the demo on the same system, it can be accessed at http://127.0.0.1:16080.' + #13#10 +
    '4) Follow the guidance of the web app.';
end;

procedure TFormDcauthservice.FormShow(Sender: TObject);
var
  AppPath, CertPath: string;
begin
  AppPath := ExtractFileDir(Application.ExeName);

  CertPath := AppPath + '\cert.pfx';
  if not FileExists(CertPath) then
  begin
    CertPath := ExtractFileDir(ExtractFileDir(AppPath)) + '\cert.pfx';

    if not FileExists(CertPath) then
      MessageDlg('The sample certificate file (cert.pfx) was not found at the expected location (' + CertPath +
        '). Please make sure you provide a valid certificate path in the box above manually.', mtWarning, [mbOk], 0);
  end;

  editCertFile.Text := CertPath;
end;

procedure TFormDcauthservice.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWebServer);
end;

procedure TFormDcauthservice.lServiceInfoClick(Sender: TObject);
var
  URL : string;
begin
  if not FWebServer.Active then
    ShowMessage('The web server is not running')
  else
  begin
    URL := 'http://127.0.0.1:' + IntToStr(FWebServer.Port) + '/';
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL) ;
  end;
end;

procedure TFormDcauthservice.Log(const S : string);
begin
  memoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + S);
end;

procedure TFormDcauthservice.LogAsync(const S : string);
var
  P : PWideChar;
begin
  GetMem(P, (Length(S) + 1) * 2);
  StrPCopy(P, S);
  PostMessage(Application.MainFormHandle, WM_LOG, NativeUInt(P), 0);
end;

procedure TFormDcauthservice.LogCapture(var Msg : TMessage);
var
  S : string;
  P : PWideChar;
begin
  P := PWideChar(Msg.WParam);
  S := StrPas(P);
  FreeMem(P);
  Log(S);
end;

function TFormDcauthservice.SignAsyncRequest(const Request : string): string;
var
  DCAuth : TsbxDCAuth;
begin
  {
    This method accepts a DCAuth request and produces the matching response.

    The DCAuth control takes the following steps when processing the request:
     - validates the integrity of the request using its KeyID and KeySecret,
     - extracts the document hash from the request,
     - signs the hash with the provided certificate,
     - incorporates the signed hash, along with any certificates (if requested) into
       the response state, and returns it.
  }

  DCAuth := TsbxDCAuth.Create(nil);
  try
    // KeyID and KeySecret are arbitrary strings (the longer, the better),
    // but they must match those used by the signing object (e.g. PDFSigner).
    DCAuth.KeyId := FKeyId;
    DCAuth.KeySecret := FKeySecret;

    // Setting up the signing certificate and its password.
    mgr.ImportFromFile(FCertFile, FCertPassword);
    DCAuth.SigningCertificate := mgr.Certificate;

    // Providing the async request.
    DCAuth.Input := Request;

    // Telling DCAuth to process it.
    DCAuth.ProcessRequest;

    // Grabbing the response state.
    Result := DCAuth.Output;
  finally
    FreeAndNil(DCAuth);
  end;
end;

function TFormDcauthservice.SignPKCS1Hash(const Hash : TBytes): TBytes;
var
  Crypto : TsbxPublicKeyCrypto;
  Mgr : TsbxCertificateManager;
  KeyMgr : TsbxCryptoKeyManager;
begin
  {
     This method takes a hash and signs it using the private key contained
     in the certificate. This method works in "java script signing" (JSON)
     mode, and does not involve DCAuth async requests and responses.
  }
  Mgr := TsbxCertificateManager.Create(nil);
  try
    // Loading the certificate
    Mgr.ImportFromFile(FCertFile, FCertPassword);

    // Extracting the signing key using the CryptoKeyManager object
    KeyMgr := TsbxCryptoKeyManager.Create(nil);
    try
      KeyMgr.Certificate := Mgr.Certificate;
      KeyMgr.ImportFromCert;

      // Signing the hash
      Crypto := TsbxPublicKeyCrypto.Create(nil);
      try
        Crypto.Key := KeyMgr.Key;
        Crypto.HashAlgorithm := 'SHA256';
        Crypto.InputIsHash := true;
        Crypto.InputEncoding := TsbxpublickeycryptoInputEncodings.cetBinary;
        Crypto.OutputEncoding := TsbxpublickeycryptoOutputEncodings.cetBinary;
        Result := Crypto.Sign(Hash, true);
      finally
        FreeAndNil(Crypto);
      end;
    finally
      FreeAndNil(KeyMgr);
    end;
  finally
    FreeAndNil(Mgr);
  end;
end;

function TFormDcauthservice.SignPKCS7Hash(const Hash : TBytes): TBytes;
var
  Signer : TsbxCAdESSigner;
  Mgr : TsbxCertificateManager;
begin
  {
     This method takes a hash and signs it using the private key contained
     in the certificate. This method works in "java script signing" (JSON)
     mode, and does not involve DCAuth async requests and responses.
  }
  Mgr := TsbxCertificateManager.Create(nil);
  try
    // Loading the certificate
    Mgr.ImportFromFile(FCertFile, FCertPassword);

    // Signing the hash
    Signer := TsbxCAdESSigner.Create(nil);
    try
      Signer.SigningCertificate := Mgr.Certificate;
      Signer.Config('InputIsHash=true');
      Signer.RevocationCheck := TsbxcadessignerRevocationChecks.crcNone;
      Signer.IgnoreChainValidationErrors := true;
      Signer.InputBytes := Hash;
      Signer.Detached := true;

      Signer.NewSignature.HashAlgorithm := 'SHA256';
      Signer.NewSignature.Level := aslBES;

      Signer.Sign();

      Result := Signer.OutputBytes;
    finally
      FreeAndNil(Signer);
    end;
  finally
    FreeAndNil(Mgr);
  end;
end;

procedure TFormDcauthservice.WelcomePage(ConnectionID : Int64);
const
  PageTpl =
    '<html>' + CrLf +
    '<head><title>DCAuth demo: welcome</title></head>' + CrLf +
    '<body style="font-family: sans-serif">' + CrLf +
    '  <h1>DCAuth demo: the local signing service</h1>' + CrLf +
    '  <h2>Welcome to DCAuth demo</h2>' + CrLf +
    '  This is an information page of the DCAuth signing service. ' + CrLf +
    '  This web service is supposed to run in environment reachable ' + CrLf +
    '  by the user''s browser - either on their workstation, or on their local' + CrLf +
    '  network.' + CrLf +
    '  <p>' + CrLf +
    '  The service listens to async requests forwarded by the browser using ' + CrLf +
    '  the Javascript piece embedded into the web page produced by the signing web app. ' + CrLf +
    '  It is supposed to run invisibly and should not be normally used directly by humans. ' + CrLf +
    '  This page is only provided for debug purposes and as a service health check. ' + CrLf +
    '  <font color="green">The service is ready to accept requests</font> and is configured to use the following parameters: ' + CrLf +
    '  <p> ' + CrLf +
    '  <ul> ' + CrLf +
    '    <li>Signing endpoint: %s' + CrLf +
    '    <li>Certificate: %s' + CrLf +
    '    <li>Key ID: %s' + CrLf +
    '    <li>Key Secret: <i>hidden</i>' + CrLf +
    '  </ul> ' + CrLf +
    '  <p> ' + CrLf +
    '  To evaluate the demo, please follow the steps below: ' + CrLf +
    '  <p>' + CrLf +
    '  <ol>' + CrLf +
    '    <li>Make sure the web application (the second part of the demo) is running. ' + CrLf +
    '      You can run it on the same or a different computer. Make sure the web app ' + CrLf +
    '      uses the right signing endpoint URI (host + resource), and the same Key ID and Key Secret' + CrLf +
    '      as used by this service.' + CrLf +
    '    <li>Use your browser to navigate to the web application. In most demo variants, the web application' + CrLf +
    '      listens on http://127.0.0.1:16080, unless modified by the user.' + CrLf +
    '    <li>Follow the instructions of the web application to initiate the signing.' + CrLf +
    '    <li>At some point the web app will lead you to the page that includes the async request.' + CrLf +
    '      When asked, confirm that you want to submit that request to the DCAuth service (THIS service).' + CrLf +
    '    <li>The DCAuth service (THIS service) will then sign the request and produce the response.' + CrLf +
    '      This will happen behind the scenes in your browser, which will use Javascript to conduct the exchange.' + CrLf +
    '    <li>If asked, confirm that you are happy to submit that response back to the web app.' + CrLf +
    '    <li>The web application will then finalize the signing and produce the signed document.' + CrLf +
    '  </ol>' + CrLf +
    '  <p>IMPORTANT NOTE: Please read the comment about CORS requirements in the body of the SignPage() method. ' + CrLf +
    '  In particular, this service MUST use HTTPS if the web application uses it. ' + CrLf +
    '</body>' + CrLf +
    '</html>';
var
  Body : string;
begin
  LogAsync('Producing the welcome page');

  Body := Format(PageTpl, [FSignEndpoint, FCertFile, FKeyID]);
  FWebServer.SetResponseStatus(ConnectionID, 200);
  FWebServer.SetResponseString(ConnectionID, Body, 'text/html', '');
end;

procedure TFormDcauthservice.SignPage(ConnectionID : Int64);
var
  Par : string;
  AsyncReq, AsyncResp : string;
begin
  // This is not a "page" per se, but rather an XML body containing the DCAuth async response.
  // If the response can't be provided for any reason, a 404 error is returned.
  LogAsync('Starting the signing handler.');

  try
    // Obtaining the async request, which is passed as a part of a multipart HTTP request
    Par := FWebServer.GetRequestString(ConnectionID, 'parts[0]');

    LogAsync('Received what seems to be an async request (' + IntToStr(Length(Par)) + ' bytes). Initiating the DCAuth signing routine.');

    // The async request comes in URLEncoded form: we need to decode it first.
    // (the web app has to use URLEncode to safely embed the request's XML data into its HTML page)
    AsyncReq := TNetEncoding.URL.Decode(Par);

    // Extracting and signing the request. If anything goes wrong, an exception
    // will be thrown, taking us to the 'except' handler below.
    AsyncResp := SignAsyncRequest(AsyncReq);

    LogAsync('Signing completed; the async response is ' + IntToStr(Length(AsyncResp)) + ' bytes long.');
    LogAsync('Producing the response.');

    // Important note: browsers are increasingly tight as to what endpoints
    // you can make Javascript requests to. This is known as CORS (https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS).
    // In many cases localhost endpoints are under severe restrictions, as are
    // non-HTTPS locations accessed from HTTPS pages. The below headers therefore
    // must be included in the response: the browsers will refuse working with it otherwise.
    // Also, if your web application should run over HTTPS (in the sample it runs over HTTP),
    // you must change the implementation of this service to use HTTPS too, and
    // make sure the TLS certificate it uses is trusted by the local system.

    // This extra CORS header tells the browser that we are happy to accept requests
    // from any page.
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Origin', '*');
    // If required, the origin can be restricted using the following parameters:
    // FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Origin', 'https://mysite.com');
    // FWebServer.SetResponseHeader(ConnectionId, 'Vary', 'Origin');

    // These extra CORS headers tell what else we are happy to accept
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Methods', 'POST, GET');
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Headers', 'Authorization, Content-Type');
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Credentials', 'true');

    // Providing the response status and body
    FWebServer.SetResponseStatus(ConnectionID, 200);
    FWebServer.SetResponseString(ConnectionID, AsyncResp, 'text/xml', '');
  except
    on E : Exception do
    begin
      LogAsync('Exception: ' + E.Message);
      ErrorPage(ConnectionId, E.Message);
    end;
  end;
end;

procedure TFormDcauthservice.SignPageJSMode(ConnectionID : Int64; IsPKCS7 : boolean);
var
  Par : string;
  SigBytes, HashBytes : TBytes;
  Sig : string;

begin
  // This is not a "page" either. This endpoint outputs a hex-encoded signature value.
  // If the response can't be provided for any reason, a 404 error is returned.
  LogAsync('Starting the java script signing handler.');

  try
    // Obtaining the hash to sign, which is passed as a part of a multipart HTTP request.
    Par := FWebServer.GetRequestString(ConnectionID, 'parts[0]');

    // The hash comes in hex-encoded form
    HashBytes := HexDecode(Par);

    LogAsync('Received a hash to be signed (' + IntToStr(Length(HashBytes)) + ' bytes). Initiating the plain (non-DCAuth) signing routine.');

    // Extracting and signing the request. If anything goes wrong, an exception
    // will be thrown, taking us to the 'except' handler below.
    if IsPKCS7 then
      SigBytes := SignPKCS7Hash(HashBytes)
    else
      SigBytes := SignPKCS1Hash(HashBytes);

    LogAsync('Signing completed; the signature is ' + IntToStr(Length(SigBytes)) + ' bytes long.');
    LogAsync('Producing the response.');

    // Converting signature to hex
    Sig := HexEncode(SigBytes);

    // See note about CORS in the SignPage() method implementation above.
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Origin', '*');
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Methods', 'POST, GET');
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Headers', 'Authorization, Content-Type');
    FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Credentials', 'true');

    // Providing the response status and body
    FWebServer.SetResponseStatus(ConnectionID, 200);
    FWebServer.SetResponseString(ConnectionID, Sig, 'text/plain', '');
  except
    on E : Exception do
    begin
      LogAsync('Exception: ' + E.Message);
      ErrorPage(ConnectionId, E.Message);
    end;
  end;
end;

procedure TFormDcauthservice.ErrorPage(ConnectionID : Int64; const ErrorMsg : string);
const
  PageTpl =
    '<html>' + CrLf +
    '<head><title>DCAuth demo: error</title></head>' + CrLf +
    '<body style="font-family: sans-serif">' + CrLf +
    '  <h1>DCAuth demo: the local signing service</h1>' + CrLf +
    '  <h2>Error</h2>' + CrLf +
    '  The following error has happened when trying to complete the operation: ' + CrLf +
    '  <p>%s.' + CrLf +
    '</body>' + CrLf +
    '</html>';
var
  Body : string;
begin
  LogAsync('Producing the error page with message "' + ErrorMsg + '"');

  // CORS headers (see comment above)
  FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Origin', '*');
  FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Methods', 'POST, GET');
  FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Headers', 'Authorization, Content-Type');
  FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Credentials', 'true');

  Body := Format(PageTpl, [ErrorMsg]);
  FWebServer.SetResponseStatus(ConnectionID, 404);
  FWebServer.SetResponseString(ConnectionID, Body, 'text/html', '');
end;

procedure TFormDcauthservice.HandleServerGetRequest(Sender: TObject; ConnectionID: Int64;
  const URI: String; var Handled: Boolean);
begin
  // This event handler fires when the web service receives a GET request.
  // Normally the DCAuth signing service should only respond to POSTs coming
  // to its signing endpoint. In our demo service we also respond to GETs
  // with a welcome message/health status. This is not required in real-life
  // service.

  LogAsync('Received GET from connection ' + IntToStr(ConnectionID) + ' for ' + URI);

  WelcomePage(ConnectionID);

  Handled := true;
end;

procedure TFormDcauthservice.HandleServerPostRequest(Sender: TObject; ConnectionID: Int64;
  const URI: String; var Handled: Boolean);
begin
  // This event handler fires when the web service receives a POST request.
  // The web app's Javascript uses POST to relay async requests to the signing
  // service. We filter the incoming request by the sign endpoint; requests
  // to any other locations result in a welcome page.

  LogAsync('Received POST from connection ' + IntToStr(ConnectionID) + ' for ' + URI);

  if CompareStr(URI, FSignEndpoint + PKCS1Format) = 0 then
  begin
    if cbEnableJSMode.Checked then
      SignPageJSMode(ConnectionID, false)
    else
      ErrorPage(ConnectionID, 'Aunctionality not supported');
  end
  else
  if CompareStr(URI, FSignEndpoint + PKCS7Format) = 0 then
  begin
    if cbEnableJSMode.Checked then
      SignPageJSMode(ConnectionID, true)
    else
      ErrorPage(ConnectionID, 'Aunctionality not supported');
  end
  else
  if CompareStr(URI, FSignEndpoint) = 0 then
    SignPage(ConnectionID)
  else
    WelcomePage(ConnectionID);

  Handled := true;
end;

end.
