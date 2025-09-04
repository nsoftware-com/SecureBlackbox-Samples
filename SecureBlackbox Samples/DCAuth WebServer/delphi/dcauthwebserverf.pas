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
unit dcauthwebserverf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage, NetEncoding,
  sbxHttpServer, sbxCertificateManager, sbxPDFSigner, sbxTypes;

const
  WM_LOG = WM_USER + 1;

  PKCS1Format = '?format=pkcs1';
  PKCS7Format = '?format=pkcs7';

type
  TFormDcauthwebserver = class(TForm)
    Label1: TLabel;
    editPort: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    editKeyID: TEdit;
    editKeySecret: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    editCertFile: TEdit;
    Panel1: TPanel;
    Label7: TLabel;
    editPreSigningEndpoint: TEdit;
    Label8: TLabel;
    editCompletionEndpoint: TEdit;
    Panel2: TPanel;
    rbPKCS1: TRadioButton;
    rbPKCS7: TRadioButton;
    Label10: TLabel;
    Label11: TLabel;
    btnStart: TButton;
    btnStop: TButton;
    memoLog: TMemo;
    Label12: TLabel;
    Label13: TLabel;
    editPreSignedDocFolder: TEdit;
    Label14: TLabel;
    editSigningServiceURL: TEdit;
    Label15: TLabel;
    editSignedDocFolder: TEdit;
    Label16: TLabel;
    editTSA: TEdit;
    Label9: TLabel;
    iInfo: TImage;
    cbJavascriptMode: TCheckBox;
    Label17: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    // The web server that listens to relayed async requests from the browser.
    FWebServer : TsbxHttpServer;

    // The certificate manager to load the public certificate
    FCertManager : TsbxCertificateManager;

    // The web server endpoints: the pre-signing one, the completion one, and the one that flushes the signed files.
    FPreSigningEndpoint : string;
    FCompletionEndpoint : string;
    FGetFileEndpoint : string;

    // The URI of the signing service. This will be embedded into the web page,
    // and used by the Javascript to contact the service. Make sure this matches
    // the one configured in the signing service piece.
    FSigningServiceURL : string;

    // A local (to the web app) directory to keep the pre-signed files.
    FPreSignedDocFolder : string;

    // A local (to the web app) directory to keep the signed files.
    FSignedDocFolder : string;

    // Whether to use PKCS7 (true) or PKCS1 (false) DCAuth method.
    FPKCS7Method : boolean;

    // Whether to use Javascript.
    FJSMode : boolean;

    // The DCAuth authentication credentials: KeyID and KeySecret.
    // These two should match the ones used by the signing service.
    FKeyID : string;
    FKeySecret : string;

    // The URI of the timestamping service.
    FTSAURL : string;

    // Path of dcauth java script
    FDCAuthJSPath : string;

    // Logging methods
    procedure Log(const S : string);
    procedure LogAsync(const S : string);
    procedure LogCapture(var Msg : TMessage); message WM_LOG;

    // Pre-signs the document and outputs a web page that contains the async request
    // and the Javascript that can relay it to the signing service. This is produced
    // in PKCS#1 and PKCS#7 modes in response to a POST from the welcome page
    // containing the unsigned document.
    procedure PreSigningPage(ConnectionID: Int64);

    // Pre-signs the document and outputs a web page that contains the async request
    // in JSON format.
    procedure PreSigningPageJSMode(ConnectionID: Int64);

    // Outputs the DCAuth Javascript file.
    procedure FlushDCAuthJS(ConnectionID: Int64);

    // Completes the signature and saves a fully signed document. This is produced
    // in response to a Javascript POST containing the async response from the pre-signing page.
    procedure CompletionPage(ConnectionID: Int64);

    // Outputs welcome/information page. It is only used for debug/testing purposes,
    // a live signing service does not need it.
    procedure WelcomePage(ConnectionID : Int64);

    // Just flushes the signed PDF file.
    procedure FlushFile(ConnectionID : Int64);

    // Outputs the error page
    procedure ErrorPage(ConnectionID : Int64; const ErrorMsg : string);

    // Implements the pre-signing logic. Takes an unsigned PDF on input,
    // saves the pre-signed copy in the local directory, returns the async request
    // and a randomly generated DocID for the document.
    function PreSignDocument(const DocData : TBytes; var DocID : string): string;

    // Implements the completion logic. Takes an async response on input,
    // collects the pre-signed copy from a local directory, embeds the signature,
    // and saves the resulting document in another local directory.
    procedure CompleteSigning(const AsyncResponse: string; var DocID : string);

    // GET and POST request handlers for the web server component.
    procedure HandleServerGetRequest(Sender: TObject; ConnectionID: Int64; const URI: String;
      var Handled: Boolean);
    procedure HandleServerPostRequest(Sender: TObject; ConnectionID: Int64; const URI: String;
      var Handled: Boolean);
  public
    { Public declarations }
  end;

var
  FormDcauthwebserver: TFormDcauthwebserver;

implementation

{$R *.dfm}

const
  CrLf = #13#10;

procedure TFormDcauthwebserver.btnStartClick(Sender: TObject);
begin
  if FWebServer.Active then
    ShowMessage('Please stop the server first!')
  else
  begin
    { Saving DCAuth protocol parameters }

    // The endpoints (relative to the local host)
    FPreSigningEndpoint := editPreSigningEndpoint.Text;
    FCompletionEndpoint := editCompletionEndpoint.Text;
    FGetFileEndpoint := '/getfile';

    // Signing service URL: make sure to specify it from the perspective
    // of the systems where the web app will run, not this web app system.
    // This URL is defined by the configuration of the second part of the sample
    // (the DCAuth signing service), which has the default setting of http://127.0.0.1:17080/sign.
    FSigningServiceURL := editSigningServiceURL.Text;

    // Authentication credentials: should match those used by the DCAuth signing service
    FKeyID := editKeyID.Text;
    FKeySecret := editKeySecret.Text;

    // Local folders to keep the presigned and signed documents in
    FPreSignedDocFolder := editPreSignedDocFolder.Text;
    FSignedDocFolder := editSignedDocFolder.Text;

    if not DirectoryExists(FPreSignedDocFolder) then
      CreateDir(FPreSignedDocFolder);

    if not DirectoryExists(FSignedDocFolder) then
      CreateDir(FSignedDocFolder);

    // The timestamping service URI
    FTSAURL := editTSA.Text;

    // The DCAuth method: PKCS1 or PKCS7
    FPKCS7Method := rbPKCS7.Checked;

    // Use javascript
    FJSMode := cbJavascriptMode.Checked;

    // Loading the certificate. The certificate is mandatory if PKCS#1 method
    // is used. It may only be needed with PKCS#7 method if you want to fill
    // the signature widget texts basing on the information contained in it.
    FCertManager.Certificate := nil;
    if FileExists(editCertFile.Text) then
    begin
      try
        FCertManager.ImportFromFile(editCertFile.Text, '');
      except
        on E : Exception do
          Log('Failed to load the certificate: ' + E.Message);
      end;
    end;

    // We can't use the PKCS#1 method if there is no public certificate on the
    // web application's side, so switching to PKCS#7 if the certificate is absent.
    if (FCertManager.Certificate = nil) and (not FPKCS7Method) then
    begin
      FPKCS7Method := true;
      rbPKCS7.Checked := true;
      ShowMessage('A certificate is required for DCAuth PKCS#1 method to work. The method has been switched to PKCS#7.');
    end;

    { Preparing the web server }

    // Port to listen on
    FWebServer.Port := StrToIntDef(editPort.Text, 8080);

    // No TLS in this simple demo
    FWebServer.TLSSettings.TLSMode := smNoTLS;

    // HTTP request handlers: we want to intercept GETs and POSTs
    FWebServer.OnGetRequest := HandleServerGetRequest;
    FWebServer.OnPostRequest := HandleServerPostRequest;

    // Starting the server
    FWebServer.Start;

    Log('The web server has started');
  end;
end;

procedure TFormDcauthwebserver.btnStopClick(Sender: TObject);
begin
  if not FWebServer.Active then
    ShowMessage('The server is not running')
  else
  begin
    FWebServer.Stop;

    Log('The web server has stopped');
  end;
end;

procedure TFormDcauthwebserver.Log(const S : string);
begin
  memoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + S);
end;

procedure TFormDcauthwebserver.LogAsync(const S : string);
var
  P : PWideChar;
begin
  GetMem(P, (Length(S) + 1) * 2);
  StrPCopy(P, S);
  PostMessage(Application.MainFormHandle, WM_LOG, NativeUInt(P), 0);
end;

procedure TFormDcauthwebserver.LogCapture(var Msg : TMessage);
var
  S : string;
  P : PWideChar;
begin
  P := PWideChar(Msg.WParam);
  S := StrPas(P);
  FreeMem(P);
  Log(S);
end;

procedure TFormDcauthwebserver.FormCreate(Sender: TObject);
begin
  FWebServer := TsbxHttpServer.Create(nil);
  FCertManager := TsbxCertificateManager.Create(nil);

  iInfo.Hint := 'To evaluate the demo, please do the following: ' + #13#10 + #13#10 +
    '1) Run this application, set up the parameters of the web app (if needed), and start the web app.' + #13#10 +
    '2) Run the second part of the demo (the signing service) on this or a different system.' + #13#10 +
    '3) On the "signing service" system, navigate to this web application in the browser. In default configuration, and when' + #13#10 +
    '    running both parts of the demo on the same system, it can be accessed at http://127.0.0.1:16080.' + #13#10 +
    '4) Follow the guidance of the web app.';

  editPreSignedDocFolder.Text := GetEnvironmentVariable('TEMP') + '\presigned';
  editSignedDocFolder.Text := GetEnvironmentVariable('TEMP') + '\signed';
end;

procedure TFormDcauthwebserver.FormShow(Sender: TObject);
var
  AppPath, CertPath: string;
begin
  AppPath := ExtractFileDir(Application.ExeName);

  CertPath := AppPath + '\cert.cer';
  if not FileExists(CertPath) then
  begin
    CertPath := ExtractFileDir(ExtractFileDir(AppPath)) + '\cert.cer';

    if not FileExists(CertPath) then
      MessageDlg('The sample certificate file (cert.cer) was not found at the expected location (' + CertPath +
        '). Please make sure you provide a valid certificate path in the box above manually.', mtWarning, [mbOk], 0);
  end;

  editCertFile.Text := CertPath;

  FDCAuthJSPath := AppPath + '\dcauth.js';
  if not FileExists(FDCAuthJSPath) then
  begin
    FDCAuthJSPath := ExtractFileDir(ExtractFileDir(AppPath)) + '\dcauth.js';

    if not FileExists(FDCAuthJSPath) then
    begin
      MessageDlg('The dc auth java scrypt file (dcauth.js) was not found at the expected location (' + FDCAuthJSPath +
        ').', mtWarning, [mbOk], 0);
      cbJavascriptMode.Enabled := false;
    end;
  end;
end;

procedure TFormDcauthwebserver.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWebServer);
  FreeAndNil(FCertManager);
end;

function TFormDcauthwebserver.PreSignDocument(const DocData : TBytes; var DocID : string): string;
var
  Signer : TsbxPDFSigner;
begin
  {
    This method pre-signs an unsigned document and produces the async request.

    Specifically, the SignAsyncBegin call performs the following steps:
     - calculates the document hash and prepares any necessary signature elements,
     - packs the hash into a DCAuth async request and signs it with KeyID and KeySecret,
     - generates a pre-signed version of the document, which is basically a
       document with a placeholder for the future signature.
  }

  // Generating a unique document ID.
  repeat
    DocID := IntToHex(GetTickCount(), 8);
  until not FileExists(FPreSignedDocFolder + '\' + DocID);

  // Running the pre-signing routine.
  Signer := TsbxPDFSigner.Create(nil);
  try
    // Assigning the unsigned document
    Signer.InputBytes := DocData;

    // Cancelling any chain validation since we are using a test certificate
    Signer.IgnoreChainValidationErrors := true;
    Signer.Config('AutoCollectRevocationInfo=false');

    // For the same reason setting the signature level to BES
    // (it is unlikely that the test certificate has any proper chain)
    Signer.NewSignature.Level := paslBES;

    // KeyID and KeySecret are arbitrary strings (the longer, the better),
    // but they must match those used by the signing service on the other side.
    Signer.ExternalCrypto.KeyID := FKeyID;
    Signer.ExternalCrypto.KeySecret := FKeySecret;

    // Specifying the signing method
    if FPKCS7Method then
      Signer.ExternalCrypto.Method := TsbxpdfsignerExternalCryptoMethods.asmdPKCS7
    else
      Signer.ExternalCrypto.Method := TsbxpdfsignerExternalCryptoMethods.asmdPKCS1;

    if FJSMode then
      Signer.ExternalCrypto.Mode := TsbxpdfsignerExternalCryptoModes.ecmDCAuthJSON
    else
      Signer.ExternalCrypto.Mode := TsbxpdfsignerExternalCryptoModes.ecmDCAuth;

    // the signing certificate is REQUIRED for PKCS#1, but optional for PKCS#7
    Signer.SigningCertificate := FCertManager.Certificate;

    // If the certificate was not provided (meaning PKCS#7 mode is used),
    // we need to tune-up the signature widget manually. If the certificate
    // is available, the signer information for the widget will be automatically
    // generated from the values contained in it.
    if Signer.SigningCertificate = nil then
    begin
      Signer.Widget.AlgorithmCaption := 'RSA';
      Signer.Widget.AlgorithmInfo := 'Strong Asymmetric Algorithm';
      Signer.Widget.Header := 'Good signature';
      Signer.Widget.SignerCaption := 'Signed by: ';
      Signer.Widget.SignerInfo := 'Trusted Signer';
    end;

    // Including DocID in the request as 'user data': DCAuth will mirror it
    // in its async response, and we will use it on the completion stage to identify
    // the document the response corresponds to.
    Signer.ExternalCrypto.Data := DocID;

    // Setting the TSA service URL. Even though the TSA is only used on the
    // completion stage, we must assign it on the pre-signing stage too,
    // as PDFSigner uses this knowledge when calculating the size of the
    // signature window.
    Signer.TimestampServer := FTSAURL;

    // Pre-signing the document.
    Result := Signer.SignAsyncBegin();

    // Saving the pre-signed document to a local directory.
    with TFileStream.Create(FPreSignedDocFolder + '\' + DocID, fmCreate) do
      try
        Write(Signer.OutputBytes[0], Length(Signer.OutputBytes));
      finally
        Free;
      end;
  finally
    FreeAndNil(Signer);
  end;
end;

procedure TFormDcauthwebserver.CompleteSigning(const AsyncResponse: string; var DocID : string);
var
  Signer : TsbxPDFSigner;
  PreSignedDoc : TBytes;
begin
  {
    This method completes the signing by inserting the signature into the earlier pre-signed document.

    Specifically, the SignAsyncEnd call performs the following steps:
     - extracts the signature from the async response,
     - validates its integrity,
     - embeds the signature, along with any other necessary elements, to the signature,
     - if the TSA URL was provided, timestamps the created signature.
  }

  Signer := TsbxPDFSigner.Create(nil);
  try
    if FJSMode then
      Signer.ExternalCrypto.Mode := TsbxpdfsignerExternalCryptoModes.ecmDCAuthJSON
    else
      Signer.ExternalCrypto.Mode := TsbxpdfsignerExternalCryptoModes.ecmDCAuth;

    // Extracting the DocID from the 'user data' mirrored by the DCAuth
    // service in its async response.
    DocID := Signer.ExtractAsyncData(AsyncResponse);

    // Checking if a pre-signed file with such DocID exists.
    if not FileExists(FPreSignedDocFolder + '\' + DocID) then
      raise Exception.Create('Pre-signed file not found');

    // Loading the pre-signed document.
    with TFileStream.Create(FPreSignedDocFolder + '\' + DocID, fmOpenRead or fmShareDenyWrite) do
      try
        SetLength(PreSignedDoc, Size);
        Read(PreSignedDoc[0], Length(PreSignedDoc));
      finally
        Free;
      end;

    // Assigning the pre-signed document
    Signer.InputBytes := PreSignedDoc;

    // Cancelling any chain validation.
    Signer.IgnoreChainValidationErrors := true;

    // Assigning credentials.
    Signer.ExternalCrypto.KeyID := FKeyID;
    Signer.ExternalCrypto.KeySecret := FKeySecret;

    // Assigning the timestamping service URL. This time the signer object
    // makes real use of it.
    Signer.TimestampServer := FTSAURL;

    // Completing the signing.
    Signer.SignAsyncEnd(AsyncResponse);

    // Saving the now fully signed document to a local directory.
    with TFileStream.Create(FSignedDocFolder + '\' + DocID + '.pdf', fmCreate) do
      try
        Write(Signer.OutputBytes[0], Length(Signer.OutputBytes));
      finally
        Free;
      end;
  finally
    FreeAndNil(Signer);
  end;
end;

procedure TFormDcauthwebserver.PreSigningPage(ConnectionID: Int64);
const
  PageTpl =
    '<html>' + CrLf +
    '<head>' + CrLf +
    '  <title>DCAuth demo: pre-signing completed</title>' + CrLf +
    '  <script>' + CrLf +
    '    // The async request itself.' + CrLf +
    '    var asyncReq = "%s"; ' + CrLf +
    '' + CrLf +
    '    // The URL where to submit the async response to.' + CrLf +
    '    var completionUri = "%s"; ' + CrLf +
    '' + CrLf +
    '    // This function relays the async request to the DCAuth signing service.' + CrLf +
    '    function submitAsyncRequest() {' + CrLf +
    '      var xhr = new XMLHttpRequest();' + CrLf +
    '      xhr.open("POST", "%s", true);' + CrLf +
    '      xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");' + CrLf +
    '      xhr.onload = function() {' + CrLf +
    '        if (this.status != 200) {' + CrLf +
    '          alert("Something went wrong: " + this.response);' + CrLf +
    '        } else {' + CrLf +
    '          // Saving a copy of the response in a page element for debug purposes.' + CrLf +
    '          var respElem = document.getElementById("asyncresptext"); ' + CrLf +
    '          respElem.innerHTML = "<p>The below async response was received: ' +
    '          <p><textarea cols=\"50\" rows=\"8\">" + this.response.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/''/g, "&#39;").replace(/"/g, "&#34;") + "</textarea>";' + CrLf +
    '' + CrLf +
    '          // Reporting the success of the operation and asking the user whether we can proceed.' + CrLf +
    '          var res = confirm("A successful async response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this response back to the web application at " + completionUri + ".");' + CrLf +
    '          if (res == true) {' + CrLf +
    '            // Submitting the form to the web application''s completion endpoint.' + CrLf +
    '            document.getElementById("asyncresp").value = this.response;' + CrLf +
    '            document.getElementById("respform").submit();' + CrLf +
    '          }' + CrLf +
    '        }' + CrLf +
    '      }' + CrLf +
    '      xhr.send(asyncReq);' + CrLf +
    '    }' + CrLf +
    '  </script>' + CrLf +
    '</head>' + CrLf +
    '<body style="font-family: sans-serif">' + CrLf +
    '  <h1>DCAuth demo: the web application</h1>' + CrLf +
    '  <h2>Pre-signing completed. </h2>' + CrLf +
    '  The PDFSigner component has successfully pre-signed the provided PDF document.' + CrLf +
    '  The pre-signed document has been assigned with a DocID of %s ' + CrLf +
    '  and saved to the following location: %s.' + CrLf +
    '  <p>' + CrLf +
    '  The async request shown below has been generated as a result of the pre-signing. ' + CrLf +
    '  This has also been included in the body of this page, and will be ' + CrLf +
    '  submitted to your local signing endpoint by the embedded Javascript when you click the Sign' + CrLf +
    '  button below. The signing procedure is performed behind the scenes and ' + CrLf +
    '  is fully transparent for you.' + CrLf +
    '  <p>' + CrLf +
    '  <textarea cols="50" rows="8">%s</textarea>.' + CrLf +
    '  <p>' + CrLf +
    '  This page expects the signing endpoint to be accessible at %s.' + CrLf +
    '  If the endpoint cannot be reached at this address, the signing will fail.' + CrLf +
    '  <p>' + CrLf +
    '  If the signing completes successfully and a good async response containing' + CrLf +
    '  the signature has been received from the signing service, this page will submit ' + CrLf +
    '  it back to the web app for completion of the signing operation. The page ' + CrLf +
    '  will notify you when this happens. ' + CrLf +
    '  <p>Now click the Sign button to pass the async request to the signing service: ' + CrLf +
    '  <button onclick="submitAsyncRequest()">Sign</button>' + CrLf +
    '  <p><div id="asyncresptext"></div>' + CrLf +
    '  <form action="%s" method="post" id="respform">' + CrLf +
    '    <input type="hidden" value="" id="asyncresp" name="asyncresp" />' + CrLf +
    '  </form>' + CrLf +
    '</body>' + CrLf +
    '</html>';
var
  Buf : TBytes;
  AsyncReq : string;
  DocID : string;
  Body : string;
begin
  LogAsync('The pre-signing page handler starting.');

  try
    // Recovering the PDF document from the POST, which is passed as a part of a multipart HTTP request
    Buf := FWebServer.GetRequestBytes(ConnectionID, 'parts[0]');

    LogAsync('Received a document (' + IntToStr(Length(Buf)) + ' bytes). Initiating the pre-signing.');

    // Passing the document for pre-signing. If anything goes wrong, an exception
    // will be thrown, taking us to the 'except' handler below.
    AsyncReq := PreSignDocument(Buf, DocID);

    LogAsync('Pre-signing completed; DocID is ' + DocID + ', async request is ' + IntToStr(Length(AsyncReq)) + ' bytes long.');
    LogAsync('Producing the page with embedded async request.');

    // Forming the page by inserting a variety of variable pieces to the template.
    // Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
    // the page's syntactical safety. The signing service URLDecodes it.
    Body := Format(PageTpl, [
      TNetEncoding.URL.Encode(AsyncReq),  // async request body embedded in the Javascript.
      FCompletionEndpoint,                // completion URI embedded in the Javascript
      FSigningServiceURL,                 // the signing service URI embedded in the Javascript
      DocID,                              // DocumentID: we are only including it for informational purposes here
      FPreSignedDocFolder + '\' + DocID,  // A path to the presigned document (again, for informational purposes)
      TNetEncoding.HTML.Encode(AsyncReq), // a copy of async request to be shown on the page (HTMLEncoded for safety)
      FSigningServiceURL,                 // the signing service URI again (for informational purposes, to be shown on the page)
      FCompletionEndpoint                 // the completion URI in the form's "action" parameter.
      ]
    );

    FWebServer.SetResponseStatus(ConnectionID, 200);
    FWebServer.SetResponseString(ConnectionID, Body, 'text/html', '');
  except
    on E : Exception do
    begin
      LogAsync('Exception: ' + E.Message);
      ErrorPage(ConnectionId, E.Message);
    end;
  end;
end;

procedure TFormDcauthwebserver.PreSigningPageJSMode(ConnectionID: Int64);
const
  PageTpl =
    '<html>' + CrLf +
    '<head>' + CrLf +
    '  <title>DCAuth demo: pre-signing completed</title>' + CrLf +
    '  <script type="text/javascript" src="%s/js" ></script>' + CrLf +
    '  <script>' + CrLf +
    '    // The async request itself.' + CrLf +
    '    var asyncReq = %s; ' + CrLf +
    '' + CrLf +
    '    // The URL where to submit the async response to.' + CrLf +
    '    var completionUri = "%s"; ' + CrLf +
    '' + CrLf +
    '    try' + CrLf +
    '    {' + CrLf +
    '      var request = new DcauthAsyncRequest(asyncReq);' + CrLf +
    '    }' + CrLf +
    '    catch(err) {' + CrLf +
    '      alert("Something went wrong: " + err.message);' + CrLf +
    '    }' + CrLf +
    '    var hash = request.getHash();' + CrLf +
    '' + CrLf +
    '    // This function relays the hash to the DCAuth signing service.' + CrLf +
    '    function submitAsyncRequest() {' + CrLf +
    '      var xhr = new XMLHttpRequest();' + CrLf +
    '      xhr.open("POST", "%s", true);' + CrLf +
    '      xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");' + CrLf +
    '      xhr.onload = function() {' + CrLf +
    '        if (this.status != 200) {' + CrLf +
    '          alert("Something went wrong: " + this.response);' + CrLf +
    '        } else {' + CrLf +
    '          // Forming the response state basing on the javascript signature we received and saving it in a page element for debug purposes.' + CrLf +
    '          var response = new DcauthAsyncResponse(request);' + CrLf +
    '          response.setSignature(this.response);' + CrLf +
    '          var respElem = document.getElementById("asyncresptext"); ' + CrLf +
    '          respElem.innerHTML = "<p>The below async state was composed from the signature received from the service: ' +
    '          <p><textarea cols=\"50\" rows=\"8\">" + response.savetostring(); + "</textarea>";' + CrLf +
    '' + CrLf +
    '          // Reporting the success of the operation and asking the user whether we can proceed.' + CrLf +
    '          var res = confirm("A successful response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this signature back to the web application at " + completionUri + ".");' + CrLf +
    '          if (res == true) {' + CrLf +
    '            // Submitting the form to the web application''s completion endpoint.' + CrLf +
    '            // TODO: check/update as needed' + CrLf +
    '            document.getElementById("asyncresp").value = response.savetostring();' + CrLf +
    '            document.getElementById("respform").submit();' + CrLf +
    '          }' + CrLf +
    '        }' + CrLf +
    '      }' + CrLf +
    '      xhr.send(hash);' + CrLf +
    '    }' + CrLf +
    '  </script>' + CrLf +
    '</head>' + CrLf +
    '<body style="font-family: sans-serif" onload=''document.getElementById("reqhash").innerHTML=hash;''>' + CrLf +
    '  <h1>DCAuth demo: the web application</h1>' + CrLf +
    '  <h2>Pre-signing completed. </h2>' + CrLf +
    '  The PDFSigner component has successfully pre-signed the provided PDF document.' + CrLf +
    '  The pre-signed document has been assigned with a DocID of %s ' + CrLf +
    '  and saved to the following location: %s.' + CrLf +
    '  <p>' + CrLf +
    '  The async request shown below has been generated as a result of the pre-signing. ' + CrLf +
    '  This has also been included in the body of this page as a JSON object, and' + CrLf +
    '  pre-processed to extract the hash. The hash is: <div id="reqhash"></div>. ' + CrLf +
    '  This hash value will be submitted to your local signing endpoint by ' + CrLf +
    '  the embedded Javascript when you click the Sign' + CrLf +
    '  button below. The signing procedure is performed behind the scenes and ' + CrLf +
    '  is fully transparent for you.' + CrLf +
    '  <p>' + CrLf +
    '  <textarea cols="50" rows="8">%s</textarea>.' + CrLf +
    '  <p>' + CrLf +
    '  This page expects the signing endpoint to be accessible at %s.' + CrLf +
    '  If the endpoint cannot be reached at this address, the signing will fail.' + CrLf +
    '  <p>' + CrLf +
    '  If the signing completes successfully and a good async response containing' + CrLf +
    '  the signature has been received from the signing service, this page will submit ' + CrLf +
    '  it back to the web app for completion of the signing operation. The page ' + CrLf +
    '  will notify you when this happens. ' + CrLf +
    '  <p>Now click the Sign button to pass the hash to the signing service: ' + CrLf +
    '  <button onclick="submitAsyncRequest()">Sign</button>' + CrLf +
    '  <p><div id="asyncresptext"></div>' + CrLf +
    '  <form action="%s" method="post" id="respform">' + CrLf +
    '    <input type="hidden" value="" id="asyncresp" name="asyncresp" />' + CrLf +
    '  </form>' + CrLf +
    '</body>' + CrLf +
    '</html>';
var
  Buf : TBytes;
  AsyncReq : string;
  DocID : string;
  Body : string;
  SigningService : string;
begin
  LogAsync('The Java script pre-signing page handler starting.');

  try
    // Recovering the PDF document from the POST, which is passed as a part of a multipart HTTP request
    Buf := FWebServer.GetRequestBytes(ConnectionID, 'parts[0]');

    LogAsync('Received a document (' + IntToStr(Length(Buf)) + ' bytes). Initiating the pre-signing.');

    // Passing the document for pre-signing. If anything goes wrong, an exception
    // will be thrown, taking us to the 'except' handler below.
    AsyncReq := PreSignDocument(Buf, DocID);

    LogAsync('Pre-signing completed; DocID is ' + DocID + ', async request is ' + IntToStr(Length(AsyncReq)) + ' bytes long.');
    LogAsync('Producing the page with embedded async request.');

    if FPKCS7Method then
      SigningService := FSigningServiceURL + PKCS7Format
    else
      SigningService := FSigningServiceURL + PKCS1Format;

    // Forming the page by inserting a variety of variable pieces to the template.
    // Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
    // the page's syntactical safety. The signing service URLDecodes it.
    Body := Format(PageTpl, [
      FPreSigningEndpoint,                // to format a Javascript URI
      AsyncReq,                           // async request body in JSON, embedded in the Javascript.
      FCompletionEndpoint,                // completion URI embedded in the Javascript
      SigningService,                     // the signing service URI embedded in the Javascript
      DocID,                              // DocumentID: we are only including it for informational purposes here
      FPreSignedDocFolder + '\' + DocID,  // A path to the presigned document (again, for informational purposes)
      TNetEncoding.HTML.Encode(AsyncReq), // a copy of async request to be shown on the page (HTMLEncoded for safety)
      SigningService,                     // the signing service URI again (for informational purposes, to be shown on the page)
      FCompletionEndpoint                 // the completion URI in the form's "action" parameter.
      ]
    );

    FWebServer.SetResponseStatus(ConnectionID, 200);
    FWebServer.SetResponseString(ConnectionID, Body, 'text/html', '');
  except
    on E : Exception do
    begin
      LogAsync('Exception: ' + E.Message);
      ErrorPage(ConnectionId, E.Message);
    end;
  end;
end;

procedure TFormDcauthwebserver.FlushDCAuthJS(ConnectionID: Int64);
var
  F : TFileStream;
  Buf : TBytes;
begin
  // WARNING! THIS IS A DUMMY PLACEHOLDER. THIS NEEDS TO BE UPDATED TO LOAD
  // THE JAVASCRIPT FILE CAREFULLY.

  LogAsync('DCAuth Javascript requested.');
  try
    F := TFileStream.Create(FDCAuthJSPath, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Buf, F.Size);
      F.Read(Buf[0], Length(Buf));
    finally
      FreeAndNil(F);
    end;

    FWebServer.SetResponseStatus(ConnectionID, 200);
    FWebServer.SetResponseBytes(ConnectionID, Buf, 'text/javascript', '');
  except
    on E : Exception do
    begin
      LogAsync('Exception: ' + E.Message);
      ErrorPage(ConnectionId, E.Message);
    end;
  end;
end;

procedure TFormDcauthwebserver.CompletionPage(ConnectionID: Int64);
const
  PageTpl =
    '<html>' + CrLf +
    '<head><title>DCAuth demo: signing completed</title></head>' + CrLf +
    '<body style="font-family: sans-serif">' + CrLf +
    '  <h1>DCAuth demo: the web application</h1>' + CrLf +
    '  <h2>Signing completed successfully</h2>' + CrLf +
    '  The remote signing procedure for your document has completed successfully. ' + CrLf +
    '  Click <a href="%s" target="_blank">here</a> to download the signed document. Click <a href="/">here</a> to start again.' + CrLf +
    '</body>' + CrLf +
    '</html>';
var
  Body : string;
  Resp, DocID : string;
begin
  LogAsync('The completion page handler starting.');

  try
    // the async response is provided in a simple URLEncoded form
    Resp := FWebServer.GetRequestString(ConnectionID, 'params[0]');

    // We are using a very simple param provision procedure here not to overcomplicate things.
    // In a real-world app you can use as complex form as you like, including
    // other parameters alongside the async response.
    if Pos('asyncresp=', Resp) = 1 then
      Resp := Copy(Resp, 11, Length(Resp))
    else
      raise Exception.Create('Unexpected request: asyncresp parameter expected');

    LogAsync('Received async response(' + IntToStr(Length(Resp)) + ' bytes). Completing the signing.');

    // Completing the signing. This call return us a DocID so we could form a download link.
    CompleteSigning(Resp, DocID);

    LogAsync('Signing completed for DocID: ' + DocID);
    LogAsync('Producing the page with a link to the signed document.');

    Body := Format(PageTpl, [
      FGetFileEndpoint + '?id=' + DocID
      ]
    );

    FWebServer.SetResponseStatus(ConnectionID, 200);
    FWebServer.SetResponseString(ConnectionID, Body, 'text/html', '');
  except
    on E : Exception do
    begin
      LogAsync('Exception: ' + E.Message);
      ErrorPage(ConnectionId, E.Message);
    end;
  end;
end;

procedure TFormDcauthwebserver.WelcomePage(ConnectionID : Int64);
const
  PageTpl =
    '<html>' + CrLf +
    '<head><title>DCAuth demo: welcome</title></head>' + CrLf +
    '<body style="font-family: sans-serif">' + CrLf +
    '  <h1>DCAuth demo: the web application</h1>' + CrLf +
    '  <h2>Welcome to DCAuth demo</h2>' + CrLf +
    '  When you click the Start button, a chosen PDF document will be uploaded' + CrLf +
    '  (POST''ed) to the pre-signing page. During the generation of the pre-signing' + CrLf +
    '  page the following will happen: ' + CrLf +
    '  <ul>' + CrLf +
    '    <li>A document hash will be calculated and incorporated into an &quot;async request&quot;,' + CrLf +
    '    <li>The async request will be incorporated into the body of the produced page, ' + CrLf +
    '    <li>A pre-signed copy of the document will be saved to the special directory on the web server.' + CrLf +
    '  </ul>' + CrLf +
    '  When your browser receives the output of the pre-signing page,' + CrLf +
    '  it will run the Javascript embedded into the page to pass the async request' + CrLf +
    '  to the local signing service and ask it to sign the hash.' + CrLf +
    '  This web application is configured to assume that the signing' + CrLf +
    '  service is accessible from the browser at the following location: %s. ' + CrLf +
    '  Please make sure the signing service is active before proceeding to the ' + CrLf +
    '  pre-signing stage.' + CrLf +
    '  <p>' + CrLf +
    '  If you are using the second part of the DCAuth demo as your signing service,' + CrLf +
    '  you can check its availability by following <a href="%s" target="_blank">this link</a>' + CrLf +
    '  (the service should respond with a welcome/status page)' + CrLf +
    '  <form action="%s" method="post" enctype="multipart/form-data">' + CrLf +
    '    Please choose a PDF file to sign: <input type="file" name="pdffile" /><br />' + CrLf +
    '    <input type="submit" value="Start signing" />' + CrLf +
    '  </form>' + CrLf +
    '</body>' + CrLf +
    '</html>';
var
  Body : string;
begin
  LogAsync('Producing the welcome page');

  // Forming the page on the basis of a template
  Body := Format(PageTpl, [
    FSigningServiceURL, // the value included on the page
    FSigningServiceURL, // the link target
    FPreSigningEndPoint // the form action
  ]);

  FWebServer.SetResponseStatus(ConnectionID, 200);
  FWebServer.SetResponseString(ConnectionID, Body, 'text/html', '');
end;

procedure TFormDcauthwebserver.FlushFile(ConnectionID : Int64);
var
  ID : string;
  Data : TBytes;
begin
  LogAsync('Flushing a file');

  try
    // The id of the file is provided as the 'id' parameter
    ID := FWebServer.GetRequestString(ConnectionID, 'params[0]');

    if Pos('id=', ID) = 1 then
      ID := Copy(ID, 4, Length(ID))
    else
      raise Exception.Create('Unexpected request: id parameter expected.');


    LogAsync('File requested: ' + ID);

    if not FileExists(FSignedDocFolder + '\' + ID + '.pdf') then
      raise Exception.Create('Signed file with this ID has not been found');

    // Reading the file contents
    with TFileStream.Create(FSignedDocFolder + '\' + ID + '.pdf', fmOpenRead) do
      try
        SetLength(Data, Size);
        Read(Data[0], Length(Data));
      finally
        Free;
      end;

    // Flushing the PDF file out
    FWebServer.SetResponseStatus(ConnectionID, 200);
    FWebServer.SetResponseBytes(ConnectionID, Data, 'application/pdf', '');
  except
    on E: Exception do
    begin
      LogAsync('Exception: ' + E.Message);
      ErrorPage(ConnectionID, E.Message);
    end;
  end;
end;

procedure TFormDcauthwebserver.ErrorPage(ConnectionID : Int64; const ErrorMsg : string);
const
  PageTpl =
    '<html>' + CrLf +
    '<head><title>DCAuth demo: error</title></head>' + CrLf +
    '<body style="font-family: sans-serif">' + CrLf +
    '  <h1>DCAuth demo: the web application</h1>' + CrLf +
    '  <h2>Error</h2>' + CrLf +
    '  The following error happened when trying to complete the operation: ' + CrLf +
    '  <p><p>%s.' + CrLf +
    '  <p>Click <a href="/">here</a> to return to the main page.' + CrLf +
    '</body>' + CrLf +
    '</html>';
var
  Body : string;
begin
  LogAsync('Producing the error page with message "' + ErrorMsg + '"');

  Body := Format(PageTpl, [ErrorMsg]);
  FWebServer.SetResponseStatus(ConnectionID, 500);
  FWebServer.SetResponseString(ConnectionID, Body, 'text/html', '');
end;

procedure TFormDcauthwebserver.HandleServerGetRequest(Sender: TObject; ConnectionID: Int64; const URI: String;
  var Handled: Boolean);
begin
  // This event handler fires when the web application receives a GET request.
  // This web app can respond to two kinds of GET requests: to /getfile resource,
  // which makes it flush a signed PDF document with the requested ID,
  // or to any other resource, which makes it produce a welcome page.

  LogAsync('Received GET from connection ' + IntToStr(ConnectionID) + ' for ' + URI);

  // a special URI for the signed file
  if Pos('/getfile', URI) = 1 then
    FlushFile(ConnectionID)

  // This endpoint is triggered when the browser requests a DCAuth javascript
  // referenced from the javascript presigning page
  else if CompareStr(URI, FPreSigningEndpoint + '/js') = 0 then
    FlushDCAuthJS(ConnectionID)

  else
    // Whatever other page is requested, outputting the welcome page
    WelcomePage(ConnectionID);

  Handled := true;
end;

procedure TFormDcauthwebserver.HandleServerPostRequest(Sender: TObject; ConnectionID: Int64; const URI: String;
  var Handled: Boolean);
begin
  // This event handler fires when the web application receives a POST request.
  // The application can process POST requests to the following endpoints:
  // - the presigning endpoint (receives unsigned PDF documents, submitted by the users from the welcome page)
  // - the completion endpoint (receives async responses submitted by the Javascript from the pre-signed page)
  // - anything else, in case of which the welcome page is displayed.

  LogAsync('Received POST from connection ' + IntToStr(ConnectionID) + ' for ' + URI);

  // This endpoint is triggered when the user initiates the signing and uploads a PDF file
  if CompareStr(URI, FPreSigningEndpoint) = 0 then
  begin
    if not FJSMode then
      // the pre-signing page for the "default" DCAuth signing
      PreSigningPage(ConnectionID)
    else
      // the pre-signing page for javascript signing
      PreSigningPageJSMode(ConnectionID);
  end

  // This endpoint is triggered when the browser submits the operation result
  else if CompareStr(URI, FCompletionEndpoint) = 0 then
    CompletionPage(ConnectionID)

  // Everything else: just showing the welcome page
  else
    WelcomePage(ConnectionID);

  Handled := true;
end;

end.


