/*
 * SecureBlackbox 2024 C++ Edition - Sample Project
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include "../../include/secureblackbox.h"

namespace ArgParser {
  static char* optval(int argc, char** argv, const char* option) {
    for (int x = 0; x < argc; x++) if (!strcmp(argv[x], option) && x != argc - 1) return argv[x + 1];
    return (char*)"";
  }

  static bool optext(int argc, char** argv, const char* option) {
    for (int x = 0; x < argc; x++) if (!strcmp(argv[x], option)) return true;
    return false;
  }
};
using namespace ArgParser;

void displayHelp() {
  printf(
    "NAME\n\n"
	"  dcauthservice -- SecureBlackbox DCAuthService Demo Application\n\n"
	"SYNOPSIS\n\n"
	"  dcauthservice <listening_port> [-endpoint signing_endpoint] [-keyid key_id] [-keysecret key_secret]\n"
    "    [-cert certificate_file] [-certpass certificate_password] [-webapp] [-disablejs]\n\n"
    "DESCRIPTION\n\n"
	"  This demo application is a part of SecureBlackbox remote signing (DCAuth) demo. It runs a local \n"
	"  HTTP service that can process async requests from the web application that runs in a local browser.\n"
	"  It uses a private key residing on a local system to sign hashes included in the async requests and \n"
	"  produce async responses containing the signatures.\n\n"
	"  The web app is the second part of the demo and should be run separately, on this or different system.\n\n"
	"  To evaluate the demo, please do the following:\n"
    "    1) Run this application, set up the parameters of the service (if needed), and start the service.\n"
    "    2) Run the second part of the demo (the web application) on this or different system.\n"
    "    3) Navigate to the web application in your local browser. In default configuration, and when running both\n"
    "       parts of the demo on the same system, it can be accessed at http://127.0.0.1:16080.\n"
    "    4) Follow the guidance of the web app\n\n"
	"The options are as follows:\n\n"
    "    -endpoint     The signing endpoint.\n"
    "  The listening port and the endpoint should match those configured in the web app.\n"
    "    -keyid        The key ID.\n"
    "    -keysecret    The key Secret.\n"
    "  The Key ID and Key Secret should match those used by the signing component on the pre-signing stage\n"
    "    -cert         The certificate used to sign file.\n"
    "    -certpass     The password for the certificate.\n"
    "    -webapp       Launch demo web app at default location\n"
	"    -disablejs    Whether to disable javascript mode\n\n"
    "EXAMPLES\n"
    "    dcauthservice 17080\n\n"
    "    dcauthservice 17080 -endpoint /sign -keyid mykeyid -keysecret mykeysecret -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword -webapp\n\n"
  );
}

bool fileExists(const char *fname)
{
	_finddata_t data;
	long nFind = _findfirst(fname, &data);
	if (nFind != -1)
	{
		_findclose(nFind);

		if (data.attrib & _A_SUBDIR)
			return false;
		else
			return true;
	}
	return false;
}

// The signing endpoint on the web service - e.g. /sign, but can be anything.
char* signendpoint = "/sign";
char* signendpointPKCS1 = "/sign?format=pkcs1";
char* signendpointPKCS7 = "/sign?format=pkcs7";

// The DCAuth authentication credentials: KeyID and KeySecret.
// These two should match the ones used by the pre-signing party, such as PDF signer.
char* keyid = "mykeyid";
char* keysecret = "mykeysecret";

// The path to the signing certificate. This must have an associated private key,
// so normally it will be in .pfx format. Sometimes PEM is also used, but
// make sure it does contain the private key.
char* certfile = "cert.pfx";

// The password to decrypt the certificate or key.
char* certpass = "password";

// disable use java script mode.
boolean disablejs = false;

char* CrLf = "\r\n";
char* PKCS1Format = "?format=pkcs1";
char* PKCS7Format = "?format=pkcs7";



// HTTP request handlers: we want to intercept GETs and POSTs
class WebHTTPServer : public HTTPServer
{
private:
	static void decodeURL(char* url) {
		// Replace (in place) any %<hex><hex> sequences with the appropriate 8-bit character.
		char* cursor = url;
		while (*cursor) {
			if ((cursor[0] == '%') &&
				cursor[1] && isxdigit(cursor[1]) &&
				cursor[2] && isxdigit(cursor[2])) {
				// We saw a % followed by 2 hex digits, so we copy the literal hex value into the URL, then advance the cursor past it:
				char hex[3];
				hex[0] = cursor[1];
				hex[1] = cursor[2];
				hex[2] = '\0';
				*url++ = (char)strtol(hex, NULL, 16);
				cursor += 3;
			}
			else {
				// Common case: This is a normal character or a bogus % expression, so just copy it
				*url++ = *cursor++;
			}
		}

		*url = '\0';
	}

	// Outputs the error page
	void errorPage(int64 connectionID, int errorCode, char* errorMsg)
	{
		char* pageTpl =
			"<html>\r\n"
			"<head><title>DCAuth demo: error</title></head>\r\n"
			"<body style=\"font-family: sans-serif\">\r\n"
			"  <h1>DCAuth demo: the local signing service</h1>\r\n"
			"  <h2>Error</h2>\r\n"
			"  The following error has happened when trying to complete the operation: \r\n"
			"  <p>[%i] %s.\r\n"
			"</body>\r\n"
			"</html>";

		printf("Producing the error page with message [%i] %s\n", errorCode, errorMsg);

		// CORS headers (see comment above)
		this->SetResponseHeader(connectionID, "Access-Control-Allow-Origin", "*");
		this->SetResponseHeader(connectionID, "Access-Control-Allow-Methods", "POST, GET");
		this->SetResponseHeader(connectionID, "Access-Control-Allow-Headers", "Authorization, Content-Type");
		this->SetResponseHeader(connectionID, "Access-Control-Allow-Credentials", "true");

		char* body = new char[MAX_PATH];
		sprintf(body, pageTpl, errorCode, errorMsg);

		this->SetResponseStatus(connectionID, 404);
		this->SetResponseString(connectionID, body, "text/html", "");
	}

	// Outputs welcome/information page. It is only used for debug/testing purposes,
	// a live signing service does not need it.
	void welcomePage(int64 connectionID)
	{
		char* pageTpl =
			"<html>\r\n"
			"<head><title>DCAuth demo: welcome</title></head>\r\n"
			"<body style=\"font-family: sans-serif\">\r\n"
			"  <h1>DCAuth demo: the local signing service</h1>\r\n"
			"  <h2>Welcome to DCAuth demo</h2>\r\n"
			"  This is an information page of the DCAuth signing service. \r\n"
			"  This web service is supposed to run in environment reachable \r\n"
			"  by the user''s browser - either on their workstation, or on their local\r\n"
			"  network.\r\n"
			"  <p>\r\n"
			"  The service listens to async requests forwarded by the browser using \r\n"
			"  the Javascript piece embedded into the web page produced by the signing web app. \r\n"
			"  It is supposed to run invisibly and should not be normally used directly by humans. \r\n"
			"  This page is only provided for debug purposes and as a service health check. \r\n"
			"  <font color=\"green\">The service is ready to accept requests</font> and is configured to use the following parameters: \r\n"
			"  <p> \r\n"
			"  <ul> \r\n"
			"    <li>Signing endpoint: %s\r\n"
			"    <li>Certificate: %s\r\n"
			"    <li>Key ID: %s\r\n"
			"    <li>Key Secret: <i>hidden</i>\r\n"
			"  </ul> \r\n"
			"  <p> \r\n"
			"  To evaluate the demo, please follow the steps below: \r\n"
			"  <p>\r\n"
			"  <ol>\r\n"
			"    <li>Make sure the web application (the second part of the demo) is running. \r\n"
			"      You can run it on the same or a different computer. Make sure the web app \r\n"
			"      uses the right signing endpoint URI (host + resource), and the same Key ID and Key Secret\r\n"
			"      as used by this service.\r\n"
			"    <li>Use your browser to navigate to the web application. In most demo variants, the web application\r\n"
			"      listens on http://127.0.0.1:16080, unless modified by the user.\r\n"
			"    <li>Follow the instructions of the web application to initiate the signing.\r\n"
			"    <li>At some point the web app will lead you to the page that includes the async request.\r\n"
			"      When asked, confirm that you want to submit that request to the DCAuth service (THIS service).\r\n"
			"    <li>The DCAuth service (THIS service) will then sign the request and produce the response.\r\n"
			"      This will happen behind the scenes in your browser, which will use Javascript to conduct the exchange.\r\n"
			"    <li>If asked, confirm that you are happy to submit that response back to the web app.\r\n"
			"    <li>The web application will then finalize the signing and produce the signed document.\r\n"
			"  </ol>\r\n"
			"  <p>IMPORTANT NOTE: Please read the comment about CORS requirements in the body of the SignPage() method. \r\n"
			"  In particular, this service MUST use HTTPS if the web application uses it. \r\n"
			"</body>\r\n"
			"</html>";

		char* body = new char[MAX_PATH];
		sprintf(body, pageTpl, signendpoint, certfile, keyid);

		this->SetResponseStatus(connectionID, 200);
		this->SetResponseString(connectionID, body, "text/html", "");
	}

	// Implements the logic of the DCAuth request processing. Takes a DCAuth request
	// on input, and produces a DCAuth response that matches it.
	char* signAsyncRequest(char* request)
	{
		// This method accepts a DCAuth request and produces the matching response.

		// The DCAuth control takes the following steps when processing the request:
		//	- validates the integrity of the request using its KeyID and KeySecret,
		//	- extracts the document hash from the request,
		//	- signs the hash with the provided certificate,
		//	- incorporates the signed hash, along with any certificates (if requested) into
		//	the response state, and returns it.

		DCAuth auth;

		// KeyID and KeySecret are arbitrary strings (the longer, the better),
		// but they must match those used by the signing object (e.g. PDFSigner).
		auth.SetKeyId(keyid);
		auth.SetKeySecret(keysecret);

		// Setting up the signing certificate and its password.
		CertificateManager certmanager;
		certmanager.ImportFromFile(certfile, certpass);
		auth.SetSigningCertHandle(certmanager.GetCertHandle());

		// Providing the async request.
		auth.SetInput(request);

		// Telling DCAuth to process it.
		auth.ProcessRequest();

		// Grabbing the response state.
		return auth.GetOutput();
	}

	// Outputs the content produced by the signing endpoint. This is not really
	// an HTML page; it returns an XML document containing the async response.
	void signPage(int64 connectionID)
	{
		// This is not a "page" per se, but rather an XML body containing the DCAuth async response.
		// If the response can't be provided for any reason, a 404 error is returned.
		printf("Starting the signing handler.\n");

		// Obtaining the async request, which is passed as a part of a multipart HTTP request
		this->Config("RequestFilter=parts[0]");

		char* asyncReq = this->GetRequestString(connectionID, "");
		int res = this->GetLastErrorCode();

		this->Config("RequestFilter=");

		if (res == 0)
		{
			printf("Received what seems to be an async request (%d bytes). Initiating the DCAuth signing routine.\n", strlen(asyncReq));

			// The async request comes in URLEncoded form: we need to decode it first.
			// (the web app has to use URLEncode to safely embed the request's XML data into its HTML page)
			decodeURL(asyncReq);

			// Extracting and signing the request. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			char* asyncResp = signAsyncRequest(asyncReq);

			printf("Signing completed; the async response is %d bytes long.\n", strlen(asyncResp));
			printf("Producing the response.\n");

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
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Origin", "*");
			// If required, the origin can be restricted using the following parameters:
			// FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Origin', 'https://mysite.com');
			// FWebServer.SetResponseHeader(ConnectionId, 'Vary', 'Origin');

			// These extra CORS headers tell what else we are happy to accept
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Methods", "POST, GET");
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Headers", "Authorization, Content-Type");
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Credentials", "true");

			// Providing the response status and body
			this->SetResponseStatus(connectionID, 200);
			this->SetResponseString(connectionID, asyncResp, "text/xml", "");
		}
		else
		{
			printf("Error: [%i] %s\n", this->GetLastErrorCode(), this->GetLastError());
			errorPage(connectionID, this->GetLastErrorCode(), this->GetLastError());
		}
	}

	unsigned char* hexDecode(const char* hexstr)
	{
		size_t len = strlen(hexstr);
		size_t final_len = len / 2;
		unsigned char* chrs = (unsigned char*)malloc((final_len + 1) * sizeof(*chrs));
		for (size_t i = 0, j = 0; j < final_len; i += 2, j++)
			chrs[j] = (hexstr[i] % 32 + 9) % 25 * 16 + (hexstr[i + 1] % 32 + 9) % 25;
		chrs[final_len] = '\0';
		return chrs;
	}

	char* hexEncode(const unsigned char* data, size_t datalen) {
		size_t final_len = datalen * 2;
		char* chrs = (char *)malloc((final_len + 1) * sizeof(*chrs));
		unsigned int j = 0;
		for (j = 0; j < datalen; j++) {
			chrs[2 * j] = (data[j] >> 4) + 48;
			chrs[2 * j + 1] = (data[j] & 15) + 48;
			if (chrs[2 * j] > 57) chrs[2 * j] += 7;
			if (chrs[2 * j + 1] > 57) chrs[2 * j + 1] += 7;
		}
		chrs[2 * j] = '\0';
		return chrs;
	}

	// Takes a hash on input and produces a java script PKCS1 signature over it.
	char* signPKCS1Hash(char* hash, long connectionID, int* outSize)
	{
		//This method takes a hash and signs it using the private key contained
		//in the certificate.This method works in "java script signing" (JSON)
		//mode, and does not involve DCAuth async requests and responses.

		// Loading the certificate
		CertificateManager certmanager;
		certmanager.ImportFromFile(certfile, certpass);

		// Extracting the signing key using the CryptoKeyManager object
		CryptoKeyManager keymanager;
		keymanager.SetCertHandle(certmanager.GetCertHandle());
		keymanager.ImportFromCert();

		// Signing the hash
		PublicKeyCrypto crypto;
		crypto.SetKeyHandle(keymanager.GetKeyHandle());
		crypto.SetHashAlgorithm("SHA256");
		crypto.SetInputIsHash(true);
		crypto.SetInputEncoding(CET_BINARY);
		crypto.SetOutputEncoding(CET_BINARY);

		char* res = crypto.Sign(hash, strlen(hash), true, outSize);

		if (crypto.GetLastErrorCode() != 0)
		{
			printf("Error: [%i] %s\n", crypto.GetLastErrorCode(), crypto.GetLastError());
			errorPage(connectionID, crypto.GetLastErrorCode(), crypto.GetLastError());
		}

		return res;
	}

	// Takes a hash on input and produces a java script PKCS7 signature over it.
	char* signPKCS7Hash(char* hash, long connectionID, int* outSize)
	{
		//This method takes a hash and signs it using the private key contained
		//in the certificate.This method works in "java script signing" (JSON)
		//mode, and does not involve DCAuth async requests and responses.

		// Loading the certificate
		CertificateManager certmanager;
		certmanager.ImportFromFile(certfile, certpass);

		// Signing the hash
		CAdESSigner signer;
		signer.SetSigningCertHandle(certmanager.GetCertHandle());
		signer.SetNewSigLevel(1);
		signer.SetDetached(true);
		signer.SetNewSigHashAlgorithm("SHA256");
		signer.Config("InputIsHash=true");
		signer.SetRevocationCheck(CRC_NONE);
		signer.SetIgnoreChainValidationErrors(true);
		signer.SetInputBytes(hash, strlen(hash));

		signer.Sign();

		if (signer.GetLastErrorCode() != 0)
		{
			printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
			errorPage(connectionID, signer.GetLastErrorCode(), signer.GetLastError());
		}

		char* outBuf;
		signer.GetOutputBytes(outBuf, *outSize);

		return outBuf;
	}

	// Outputs the content produced by the java script signing endpoint. Just as SignPage(),
	// this is not an HTML page; it returns a hexadecimal-encoded signature value
	// to be placed in the async response.
	void signPageJSMode(int64 connectionID, boolean isPKCS7)
	{
		// This is not a "page" per se, but rather an XML body containing the DCAuth async response.
		// If the response can't be provided for any reason, a 404 error is returned.
		printf("Starting the java script signing handler.\n");

		// Obtaining the async request, which is passed as a part of a multipart HTTP request
		this->Config("RequestFilter=parts[0]");

		char* par = this->GetRequestString(connectionID, "");
		int res = this->GetLastErrorCode();

		this->Config("RequestFilter=");

		if (res == 0)
		{
			char* hashBytes = (char*)hexDecode(par);

			printf("Received a hash to be signed (%d bytes). Initiating the plain (non-DCAuth) signing routine.\n", strlen(hashBytes));

			// Extracting and signing the request. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			char* sigBytes;
			int outSize;
			if (isPKCS7)
				sigBytes = signPKCS7Hash(hashBytes, connectionID, &outSize);
			else
				sigBytes = signPKCS1Hash(hashBytes, connectionID, &outSize);

			printf("Signing completed; the async response is %d bytes long.\n", outSize);
			printf("Producing the response.\n");

			// Converting signature to hex
			char* sig = hexEncode((unsigned char*)sigBytes, outSize);


			// See note about CORS in the SignPage() method implementation above.
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Origin", "*");
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Methods", "POST, GET");
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Headers", "Authorization, Content-Type");
			this->SetResponseHeader(connectionID, "Access-Control-Allow-Credentials", "true");

			// Providing the response status and body
			this->SetResponseStatus(connectionID, 200);
			this->SetResponseString(connectionID, sig, "text/xml", "");
		}
		else
		{
			printf("Error: [%i] %s\n", this->GetLastErrorCode(), this->GetLastError());
			errorPage(connectionID, this->GetLastErrorCode(), this->GetLastError());
		}
	}

public:

	virtual int FireGetRequest(HTTPServerGetRequestEventParams* e)
	{
		// This event handler fires when the web service receives a GET request.
		// Normally the DCAuth signing service should only respond to POSTs coming
		// to its signing endpoint. In our demo service we also respond to GETs
		// with a welcome message/health status. This is not required in real-life
		// service.

		printf("Received GET for %s from connection %d \n", e->URI, e->ConnectionID);

		welcomePage(e->ConnectionID);

		e->Handled = true;

		return 0;
	}

	virtual int FirePostRequest(HTTPServerPostRequestEventParams* e)
	{
		// This event handler fires when the web service receives a POST request.
		// The web app's Javascript uses POST to relay async requests to the signing
		// service. We filter the incoming request by the sign endpoint; requests
		// to any other locations result in a welcome page.

		printf("Received POST for %s from connection %i \n", e->URI, e->ConnectionID);
		
		if (strcmp(e->URI, signendpointPKCS1) == 0)
		{
			if (!disablejs)
				signPageJSMode(e->ConnectionID, false);
			else
				errorPage(e->ConnectionID, 0, "Functionality not supported");
		}
		else
		if (strcmp(e->URI, signendpointPKCS7) == 0)
		{
			if (!disablejs)
				signPageJSMode(e->ConnectionID, true);
			else
				errorPage(e->ConnectionID, 0, "Functionality not supported");
		}
		else
		if (strcmp(e->URI, signendpoint) == 0)
			signPage(e->ConnectionID);
		else
			welcomePage(e->ConnectionID);

		e->Handled = true;


		return 0;
	}
};

int main(int argc, char** argv)
{
	int res;

	// The web server that listens to relayed async requests from the browser.
	WebHTTPServer webserver;

	// Validate input
	if (argc < 2) {
		displayHelp();
		getchar();
		return 1;
	}
	
	// The signing endpoint (relative to the local host, e.g. /sign)
	if (optext(argc, argv, "-endpoint"))
	{
		signendpoint = optval(argc, argv, "-endpoint");
	}

	signendpointPKCS1 = new char[strlen(signendpoint) + strlen(PKCS1Format) + 1];
	strcpy(signendpointPKCS1, signendpoint);
	strcat(signendpointPKCS1, PKCS1Format);

	signendpointPKCS7 = new char[strlen(signendpoint) + strlen(PKCS7Format) + 1];
	strcpy(signendpointPKCS7, signendpoint);
	strcat(signendpointPKCS7, PKCS7Format);

	// Authentication credentials
	if (optext(argc, argv, "-keyid"))
	{
		keyid = optval(argc, argv, "-keyid");
	}
	if (optext(argc, argv, "-keysecret"))
	{
		keysecret = optval(argc, argv, "-keysecret");
	}

	// Signing certificate parameters
	if (optext(argc, argv, "-cert"))
	{
		certfile = optval(argc, argv, "-cert");
	}
	else
	{
		// default cert file path
		char* allPath = new char[MAX_PATH];
		GetCurrentDirectory(MAX_PATH, allPath);
		certfile = new char[strlen(allPath) + strlen("\\cert.pfx") + 1];
		strcpy(certfile, allPath);
		strcat(certfile, "\\cert.pfx");
	}
	if (optext(argc, argv, "-certpass"))
	{
		certpass = optval(argc, argv, "-certpass");
	}

	// Launch demo web app at default location on start
	boolean launchwebapp = false;
	if (optext(argc, argv, "-webapp"))
	{
		launchwebapp = true;
	}

	if (optext(argc, argv, "-disablejs"))
	{
		disablejs = true;
	}

	if (!fileExists(certfile))
	{
		printf("Please make sure you provide a signing certificate that includes its private key. The service will be unable to sign async requests otherwise.\n");
		return 2;
	}

	//== Preparing the web server

	// Port to listen on
	char* endptr = NULL;
	long port = strtol(argv[1], &endptr, 10);

	if (*endptr == '\0') {
		webserver.SetPort(port);
	}
	else
	{
		// Set deafult port value
		webserver.SetPort(17080);
	}

	// No need for keep-alive connections
	webserver.SetAllowKeepAlive(false);

	// Starting the server
	res = webserver.Start();
	if (res == 0)
	{
		if (launchwebapp)
		{
			char* URL = "http://127.0.0.1:16080/";
			printf("The demo will now try to open a web application at its known default location of %s.\n", URL);
			printf("Please make sure the web application (the second part of the demo) is running.\n");
			printf("If you changed any settings in the web app, this button may fail to work, and you \n");
			printf("might need to open the application manually by typing the correct URL in your browser.\n");

			ShellExecute(0, 0, URL, 0, 0, SW_SHOW);
		}

		printf("\nThe web endpoint has started on port %d. Press enter to stop and exit.\n\n", webserver.GetPort());
		getchar();

		webserver.Stop();
		printf("The web endpoint has stopped\n");

		return 0;
	}
	else
	{
		printf("Failed to run the server, error %d\n", res);
		getchar();

		return 2;
	}
}


