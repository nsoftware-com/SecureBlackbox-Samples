/*
 * SecureBlackbox 2022 C++ Edition - Sample Project
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
#include <io.h>
#include <fstream>
#include <iostream>
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
	"  dcauthwebserver <listening_port>[-presignendpoint presigning_endpoint][-completendpoint completion_endpoint] \n"
	"    [-keyid key_id] [-keysecret key_secret] [-signurl signing_service_url] [-usepkcs7] [-usejs]\n"
    "    [-cert certificate_file] [-tsaurl timestamp_service_url] [-presign pre_signed_doc_folder] [-sign signed_doc_folder]\n\n"
    "DESCRIPTION\n\n"
	"  This demo application is a part of SecureBlackbox remote signing (DCAuth) demo. It runs a web app \n"
	"  that can work with the DCAuth signing service (the second part of the demo) to sign PDF documents remotely.\n\n"
	"  To evaluate the demo, please do the following: \n\n"
	"    1) Run this application, set up the parameters of the web app (if needed), and start the web app.\n"
	"    2) Run the second part of the demo (the signing service) on this or a different system.\n"
	"    3) On the \"signing service\" system, navigate to this web application in the browser. In default configuration, and when\n"
	"       running both parts of the demo on the same system, it can be accessed at http://127.0.0.1:16080.\n"
	"    4) Follow the guidance of the web app.\n\n"
	"The options are as follows:\n\n"
	"    -presignendpoint     The pre-signing endpoint.\n"
	"    -completendpoint     The completion endpoint.\n"
	"  Enter the port number that you want your web server to listen on, and the web endpoints for initiation and completion stages of the async signing workflow.\n"
	"    -keyid        The key ID.\n"
	"    -keysecret    The key Secret.\n"
	"    -signurl      The signing service URL on the connecting systems.\n"
	"  The Key ID and Key Secret should match those used by the signing component on the pre-signing stage\n"
	"    -usepkcs7     Whether to use PKCS7 DCAuth method.\n"
	"    -usejs        Whether to use java script mode.\n"
	"    -cert         The certificate used to sign file.\n"
	"  The PKCS#1 method requires the public part of the signing certificate to be available for the web app\n"
	"    -tsaurl       The timestamp service URL.\n"
	"    -presign      The pre-signed documents folder.\n"
	"    -sign         The signed documents folder.\n\n"
    "EXAMPLES\n"
    "    dcauthwebserver 16080\n\n"
    "    dcauthwebserver 16080 -presignendpoint /start -completendpoint /finish -keyid mykeyid -keysecret mykeysecret -signurl http://127.0.0.1:17080/sign \n"
	"        -usepkcs7 -cert C:\\certs\\mycert.pfx -tsaurl mytimestampserver -presign C:\\presignfolder\\ -sign C:\\signfolder\\ \n\n"
  );
}

// The web server endpoints: the pre-signing one, the completion one, and the one that flushes the signed files.
char* preSigningEndpoint = "/start";
char* preSigningJSEndpoint = "/start/js";
char* completionEndpoint = "/finish";
char* getFileEndpoint = "/getfile";

// The URI of the signing service. This will be embedded into the web page,
// and used by the Javascript to contact the service. Make sure this matches
// the one configured in the signing service piece.
char* signingServiceURL = "http://127.0.0.1:17080/sign";

// A local (to the web app) directory to keep the pre-signed files.
char* preSignedDocFolder = "";

// A local (to the web app) directory to keep the signed files.
char* signedDocFolder = "";

// Whether to use PKCS7 (true) or PKCS1 (false) DCAuth method.
boolean usePKCS7Method = false;

// The DCAuth authentication credentials: KeyID and KeySecret.
// These two should match the ones used by the signing service.
char* keyID = "mykeyid";
char* keySecret = "mykeysecret";

// The URI of the timestamping service.
char* TSAURL = "";

char* certfile = "cert.cer";

// Whether to use Javascript.
boolean useJSMode = false;

// Path of dcauth java script
char* dcAuthJSPath = "";

char* PKCS1Format = "?format=pkcs1";
char* PKCS7Format = "?format=pkcs7";

// HTTP request handlers: we want to intercept GETs and POSTs
class WebHTTPServer : public HTTPServer
{
private:
	char to_hex(char code) {
		static char hex[] = "0123456789abcdef";
		return hex[code & 15];
	}

	char *url_encode(const char *str) {
		char *pstr = (char*)str, *buf = (char*)malloc(strlen(str) * 3 + 1), *pbuf = buf;
		while (*pstr) {
			if (isalnum(*pstr) || *pstr == '-' || *pstr == '_' || *pstr == '.' || *pstr == '~')
				*pbuf++ = *pstr;
			else if (*pstr == ' ')
				*pbuf++ = '+';
			else
				*pbuf++ = '%', *pbuf++ = to_hex(*pstr >> 4), *pbuf++ = to_hex(*pstr & 15);
			pstr++;
		}
		*pbuf = '\0';
		return buf;
	}

	void encodeHtml(std::string& data) {
		std::string buffer;
		buffer.reserve(data.size());
		for (size_t pos = 0; pos != data.size(); ++pos) {
			switch (data[pos]) {
			case '&':  buffer.append("&amp;");       break;
			case '\"': buffer.append("&quot;");      break;
			case '\'': buffer.append("&apos;");      break;
			case '<':  buffer.append("&lt;");        break;
			case '>':  buffer.append("&gt;");        break;
			default:   buffer.append(&data[pos], 1); break;
			}
		}
		data.swap(buffer);
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

	// Outputs the error page
	void errorPage(int64 connectionID, int errorCode, char* errorMsg)
	{
		char* pageTpl =
			"<html>\r\n"
			"<head><title>DCAuth demo: error</title></head>\r\n"
			"<body style=\"font-family: sans-serif\">\r\n"
			"  <h1>DCAuth demo: the web application</h1>\r\n"
			"  <h2>Error</h2>\r\n"
			"  The following error has happened when trying to complete the operation: \r\n"
			"  <p>[%i] %s.\r\n"
			"</body>\r\n"
			"</html>";

		printf("Producing the error page with message [%i] %s\n", errorCode, errorMsg);

		char* body = new char[2000];
		sprintf(body, pageTpl, errorCode, errorMsg);

		this->SetResponseStatus(connectionID, 500);
		this->SetResponseString(connectionID, body, "text/html");
	}

	// Implements the pre-signing logic. Takes an unsigned PDF on input,
	// saves the pre-signed copy in the local directory, returns the async request
	// and a randomly generated DocID for the document.
	char* preSignDocument(char* docData, int docDataSize, char* docID, int64 connectionID)
	{
		// This method pre-signs an unsigned document and produces the async request.

		// Specifically, the SignAsyncBegin call performs the following steps:
		//	- calculates the document hash and prepares any necessary signature elements,
		//	- packs the hash into a DCAuth async request and signs it with KeyID and KeySecret,
		//	- generates a pre-signed version of the document, which is basically a
		//	  document with a placeholder for the future signature.

		// Running the pre-signing routine.
		PDFSigner signer; 

		// The certificate manager to load the public certificate
		CertificateManager certmanager;
		
		// Assigning the unsigned document
		signer.SetInputBytes(docData, docDataSize);

		// Cancelling any chain validation since we are using a test certificate
		signer.SetIgnoreChainValidationErrors(true);
		signer.Config("AutoCollectRevocationInfo=false");

		// For the same reason setting the signature level to BES
		// (it is unlikely that the test certificate has any proper chain)
		signer.SetSigLevel(PSL_BES);

		// KeyID and KeySecret are arbitrary strings (the longer, the better),
		// but they must match those used by the signing service on the other side.
		signer.SetExternalCryptoKeyID(keyID);
		signer.SetExternalCryptoKeySecret(keySecret);

		// Specifying the signing method
		if (usePKCS7Method)
			signer.SetExternalCryptoMethod(ASMD_PKCS7);
		else
			signer.SetExternalCryptoMethod(ASMD_PKCS1);

		if (useJSMode)
			signer.SetExternalCryptoMode(ECM_DCAUTHJSON);
		else
			signer.SetExternalCryptoMode(ECM_DCAUTH);

		// Loading the certificate. The certificate is mandatory if PKCS#1 method
		// is used. It may only be needed with PKCS#7 method if you want to fill
		// the signature widget texts basing on the information contained in it.
		certmanager.ImportFromFile(certfile, "");
		if (certmanager.GetLastErrorCode() == 0)
		{
			signer.SetSigningCertHandle(certmanager.GetCertHandle());
		}
		else
		{
			// We can't use the PKCS#1 method if there is no public certificate on the
			// web application's side, so switching to PKCS#7 if the certificate is absent.
			if (!usePKCS7Method)
			{
				usePKCS7Method = true;
				printf("A certificate is required for DCAuth PKCS#1 method to work. The method has been switched to PKCS#7.\n");
			}
		}

		// If the certificate was not provided (meaning PKCS#7 mode is used),
		// we need to tune-up the signature widget manually. If the certificate
		// is available, the signer information for the widget will be automatically
		// generated from the values contained in it.
		if (signer.GetSigningCertHandle() == 0)
		{
			signer.SetSigAlgorithmCaption("RSA");
			signer.SetSigAlgorithmInfo("Strong Asymmetric Algorithm");
			signer.SetSigHeader("Good signature");
			signer.SetSigSignerCaption("Signed by: ");
			signer.SetSigSignerInfo("Trusted Signer");
		}

		// Including DocID in the request as 'user data': DCAuth will mirror it
		// in its async response, and we will use it on the completion stage to identify
		// the document the response corresponds to.
		signer.SetExternalCryptoData(docID);

		// Setting the TSA service URL. Even though the TSA is only used on the
		// completion stage, we must assign it on the pre-signing stage too,
		// as PDFSigner uses this knowledge when calculating the size of the
		// signature window.
		signer.SetTimestampServer(TSAURL);

		// Pre-signing the document.
		char* res = signer.SignAsyncBegin();

		if (signer.GetLastErrorCode() == 0)
		{
			// Saving the pre-signed document to a local directory.
			int outBufSize;
			char* outBuf;
			signer.GetOutputBytes(outBuf, outBufSize);

			char* preSignedFile = new char[MAX_PATH];
			sprintf(preSignedFile, "%s\\%s", preSignedDocFolder, docID);

			FILE* file = fopen(preSignedFile, "wb");
			fwrite(outBuf, 1, outBufSize, file);
			fclose(file);
		}
		else
		{
			printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
			errorPage(connectionID, signer.GetLastErrorCode(), signer.GetLastError());
		}

		return res;
	}

	char* printRandomString(int n)
	{
		const int MAX = 26;
		char alphabet[MAX] = { 'a', 'b', 'c', 'd', 'e', 'f', 'g',
			'h', 'i', 'j', 'k', 'l', 'm', 'n',
			'o', 'p', 'q', 'r', 's', 't', 'u',
			'v', 'w', 'x', 'y', 'z' };

		char* res = new char[n+1];
		for (int i = 0; i < n; i++)
			res[i] = alphabet[rand() % MAX];
		res[n] = '\0';

		return res;
	}

	// Pre-signs the document and outputs a web page that contains the async request
	// and the Javascript that can relay it to the signing service. This is produced
	// in response to a POST from the welcome page containing the unsigned document.
	void preSigningPage(int64 connectionID)
	{
		char* pageTpl =
			"<html>\r\n"
			"<head>\r\n"
			"  <title>DCAuth demo: pre-signing completed</title>\r\n"
			"  <script>\r\n"
			"    // The async request itself.\r\n"
			"    var asyncReq = \"%s\"; \r\n"
			"\r\n"
			"    // The URL where to submit the async response to.\r\n"
			"    var completionUri = \"%s\"; \r\n"
			"\r\n"
			"    // This function relays the async request to the DCAuth signing service.\r\n"
			"    function submitAsyncRequest() {\r\n"
			"      var xhr = new XMLHttpRequest();\r\n"
			"      xhr.open(\"POST\", \"%s\", true);\r\n"
			"      xhr.setRequestHeader(\"Content-Type\", \"application/x-www-form-urlencoded\");\r\n"
			"      xhr.onload = function() {\r\n"
			"        if (this.status != 200) {\r\n"
			"          alert(\"Something went wrong: \" + this.response);\r\n"
			"        } else {\r\n"
			"          // Saving a copy of the response in a page element for debug purposes.\r\n"
			"          var respElem = document.getElementById(\"asyncresptext\"); \r\n"
			"          respElem.innerHTML = \"<p>The below async response was received: "
			"          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + this.response.replace(/&/g, \"&amp;\").replace(/</g, \"&lt;\").replace(/>/g, \"&gt;\").replace(/'/g, \"&#39;\").replace(/\"/g, \"&#34;\") + \"</textarea>\";\r\n"
			"\r\n"
			"          // Reporting the success of the operation and asking the user whether we can proceed.\r\n"
			"          var res = confirm(\"A successful async response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this response back to the web application at \" + completionUri + \".\");\r\n"
			"          if (res == true) {\r\n"
			"            // Submitting the form to the web application's completion endpoint.\r\n"
			"            document.getElementById(\"asyncresp\").value = this.response;\r\n"
			"            document.getElementById(\"respform\").submit();\r\n"
			"          }\r\n"
			"        }\r\n"
			"      }\r\n"
			"      xhr.send(asyncReq);\r\n"
			"    }\r\n"
			"  </script>\r\n"
			"</head>\r\n"
			"<body style=\"font-family: sans-serif\">\r\n"
			"  <h1>DCAuth demo: the web application</h1>\r\n"
			"  <h2>Pre-signing completed. </h2>\r\n"
			"  The PDFSigner component has successfully pre-signed the provided PDF document.\r\n"
			"  The pre-signed document has been assigned with a DocID of %s \r\n"
			"  and saved to the following location: %s.\r\n"
			"  <p>\r\n"
			"  The async request shown below has been generated as a result of the pre-signing. \r\n"
			"  This has also been included in the body of this page, and will be \r\n"
			"  submitted to your local signing endpoint by the embedded Javascript when you click the Sign\r\n"
			"  button below. The signing procedure is performed behind the scenes and \r\n"
			"  is fully transparent for you.\r\n"
			"  <p>\r\n"
			"  <textarea cols=\"50\" rows=\"8\">%s</textarea>.\r\n"
			"  <p>\r\n"
			"  This page expects the signing endpoint to be accessible at %s.\r\n"
			"  If the endpoint cannot be reached at this address, the signing will fail.\r\n"
			"  <p>\r\n"
			"  If the signing completes successfully and a good async response containing\r\n"
			"  the signature has been received from the signing service, this page will submit \r\n"
			"  it back to the web app for completion of the signing operation. The page \r\n"
			"  will notify you when this happens. \r\n"
			"  <p>Now click the Sign button to pass the async request to the signing service: \r\n"
			"  <button onclick=\"submitAsyncRequest()\">Sign</button>\r\n"
			"  <p><div id=\"asyncresptext\"></div>\r\n"
			"  <form action=\"%s\" method=\"post\" id=\"respform\">\r\n"
			"    <input type=\"hidden\" value=\"\" id=\"asyncresp\" name=\"asyncresp\" />\r\n"
			"  </form>\r\n"
			"</body>\r\n"
			"</html>";

		printf("The pre-signing page handler starting.\n");

		// Recovering the PDF document from the POST, which is passed as a part of a multipart HTTP request
		this->Config("RequestFilter=parts[0]");

		int bufSize = 0;
		char* buf = this->GetRequestBytes(connectionID, &bufSize);

		if (this->GetLastErrorCode() == 0)
		{
			this->Config("RequestFilter=");

			printf("Received a document (%d bytes). Initiating the pre-signing.\n", bufSize);

			// Generating a unique document ID.
			char* docID;
			char* preSignedFile = new char[MAX_PATH];
			do
			{
				docID = printRandomString(10);
				sprintf(preSignedFile, "%s\\%s", preSignedDocFolder, docID);
			}
			while (fileExists(preSignedFile));

			// Passing the document for pre-signing. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			char* asyncReq = preSignDocument(buf, bufSize, docID, connectionID);
			if (asyncReq == NULL)
				return;

			printf("Pre-signing completed; DocID is %s, async request is %d bytes long.\n", docID, strlen(asyncReq));
			printf("Producing the page with embedded async request.\n");

			// Forming the page by inserting a variety of variable pieces to the template.
			// Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
			// the page's syntactical safety. The signing service URLDecodes it.
			std::string asyncReqStr(asyncReq);
			encodeHtml(asyncReqStr);
			char* body = new char[4000 + 2 * strlen(asyncReq)];
			sprintf(body, pageTpl, 
				url_encode(asyncReq),  // async request body embedded in the Javascript.
				completionEndpoint,    // completion URI embedded in the Javascript
				signingServiceURL,     // the signing service URI embedded in the Javascript
				docID,                 // DocumentID: we are only including it for informational purposes here
				preSignedFile,         // A path to the presigned document (again, for informational purposes)
				asyncReqStr.c_str(),   // a copy of async request to be shown on the page (HTMLEncoded for safety)
				signingServiceURL,     // the signing service URI again (for informational purposes, to be shown on the page)
				completionEndpoint     // the completion URI in the form's "action" parameter.
			);

			this->SetResponseStatus(connectionID, 200);
			this->SetResponseString(connectionID, body, "text/html");
		}
		else
		{
			this->Config("RequestFilter=");

			printf("Error: [%i] %s\n", this->GetLastErrorCode(), this->GetLastError());
			errorPage(connectionID, this->GetLastErrorCode(), this->GetLastError());
		}
	}

	// Pre-signs the document and outputs a web page that contains the async request in JSON format.
	void preSigningPageJSMode(int64 connectionID)
	{
		char* pageTpl =
			"<html>\r\n"
			"<head>\r\n"
			"  <title>DCAuth demo: pre-signing completed</title>\r\n"
			"  <script type=\"text/javascript\" src=\"%s/js\" ></script>\r\n"
			"  <script>\r\n"
			"    // The async request itself.\r\n"
			"    var asyncReq = %s; \r\n"
			"\r\n"
			"    // The URL where to submit the async response to.\r\n"
			"    var completionUri = \"%s\"; \r\n"
			"\r\n"
			"    try\r\n"
			"    {\r\n"
			"      var request = new DcauthAsyncRequest(asyncReq);\r\n"
			"    }\r\n"
			"    catch(err) {\r\n"
			"      alert(\"Something went wrong: \" + err.message);\r\n"
			"    }\r\n"
			"    var hash = request.getHash();\r\n"
			"\r\n"
			"    // This function relays the hash to the DCAuth signing service.\r\n"
			"    function submitAsyncRequest() {\r\n"
			"      var xhr = new XMLHttpRequest();\r\n"
			"      xhr.open(\"POST\", \"%s\", true);\r\n"
			"      xhr.setRequestHeader(\"Content-Type\", \"application/x-www-form-urlencoded\");\r\n"
			"      xhr.onload = function() {\r\n"
			"        if (this.status != 200) {\r\n"
			"          alert(\"Something went wrong: \" + this.response);\r\n"
			"        } else {\r\n"
			"          // Forming the response state basing on the javascript signature we received and saving it in a page element for debug purposes.\r\n"
			"          var response = new DcauthAsyncResponse(request);\r\n"
			"          response.setSignature(this.response);\r\n"
			"          var respElem = document.getElementById(\"asyncresptext\"); \r\n"
			"          respElem.innerHTML = \"<p>The below async state was composed from the signature received from the service: "
			"          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + response.savetostring(); + \"</textarea>\";\r\n"
			"\r\n"
			"          // Reporting the success of the operation and asking the user whether we can proceed.\r\n"
			"          var res = confirm(\"A successful response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this signature back to the web application at \" + completionUri + \".\");\r\n"
			"          if (res == true) {\r\n"
			"            // Submitting the form to the web application's completion endpoint.\r\n"
			"            // TODO: check/update as needed\r\n"
			"            document.getElementById(\"asyncresp\").value = response.savetostring();\r\n"
			"            document.getElementById(\"respform\").submit();\r\n"
			"          }\r\n"
			"        }\r\n"
			"      }\r\n"
			"      xhr.send(hash);\r\n"
			"    }\r\n"
			"  </script>\r\n"
			"</head>\r\n"
			"<body style=\"font-family: sans-serif\" onload=''document.getElementById(\"reqhash\").innerHTML=hash;''>\r\n"
			"  <h1>DCAuth demo: the web application</h1>\r\n"
			"  <h2>Pre-signing completed. </h2>\r\n"
			"  The PDFSigner component has successfully pre-signed the provided PDF document.\r\n"
			"  The pre-signed document has been assigned with a DocID of %s \r\n"
			"  and saved to the following location: %s.\r\n"
			"  <p>\r\n"		
			"  The async request shown below has been generated as a result of the pre-signing. \r\n"
			"  This has also been included in the body of this page as a JSON object, and\r\n"
			"  pre-processed to extract the hash. The hash is: <div id=\"reqhash\"></div>. \r\n"
			"  This hash value will be submitted to your local signing endpoint by \r\n"
			"  the embedded Javascript when you click the Sign\r\n"
			"  button below. The signing procedure is performed behind the scenes and \r\n"
			"  is fully transparent for you.\r\n"
			"  <p>\r\n"
			"  <textarea cols=\"50\" rows=\"8\">%s</textarea>.\r\n"
			"  <p>\r\n"
			"  This page expects the signing endpoint to be accessible at %s.\r\n"
			"  If the endpoint cannot be reached at this address, the signing will fail.\r\n"
			"  <p>\r\n"
			"  If the signing completes successfully and a good async response containing\r\n"
			"  the signature has been received from the signing service, this page will submit \r\n"
			"  it back to the web app for completion of the signing operation. The page \r\n"
			"  will notify you when this happens. \r\n"
			"  <p>Now click the Sign button to pass the hash to the signing service: \r\n"
			"  <button onclick=\"submitAsyncRequest()\">Sign</button>\r\n"
			"  <p><div id=\"asyncresptext\"></div>\r\n"
			"  <form action=\"%s\" method=\"post\" id=\"respform\">\r\n"
			"    <input type=\"hidden\" value=\"\" id=\"asyncresp\" name=\"asyncresp\" />\r\n"
			"  </form>\r\n"
			"</body>\r\n"
			"</html>";

		printf("The Java script pre-signing page handler starting.\n");

		// Recovering the PDF document from the POST, which is passed as a part of a multipart HTTP request
		this->Config("RequestFilter=parts[0]");

		int bufSize = 0;
		char* buf = this->GetRequestBytes(connectionID, &bufSize);

		if (this->GetLastErrorCode() == 0)
		{
			this->Config("RequestFilter=");

			printf("Received a document (%d bytes). Initiating the pre-signing.\n", bufSize);

			// Generating a unique document ID.
			char* docID;
			char* preSignedFile = new char[MAX_PATH];
			do
			{
				docID = printRandomString(10);
				sprintf(preSignedFile, "%s\\%s", preSignedDocFolder, docID);
			} while (fileExists(preSignedFile));

			// Passing the document for pre-signing. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			char* asyncReq = preSignDocument(buf, bufSize, docID, connectionID);
			if (asyncReq == NULL)
				return;

			printf("Pre-signing completed; DocID is %s, async request is %d bytes long.\n", docID, strlen(asyncReq));
			printf("Producing the page with embedded async request.\n");
			
			char* signingService = new char[strlen(signingServiceURL) + strlen(PKCS7Format) + 1];
			if (usePKCS7Method)
				sprintf(signingService, "%s%s", signingServiceURL, PKCS7Format);
			else
				sprintf(signingService, "%s%s", signingServiceURL, PKCS1Format);

			// Forming the page by inserting a variety of variable pieces to the template.
			// Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
			// the page's syntactical safety. The signing service URLDecodes it.
			std::string asyncReqStr(asyncReq);
			encodeHtml(asyncReqStr);
			char* body = new char[8000 + 2 * strlen(asyncReq)];
			sprintf(body, pageTpl,
				preSigningEndpoint,                // to format a Javascript URI
				asyncReq,                          // async request body in JSON, embedded in the Javascript.
				completionEndpoint,                // completion URI embedded in the Javascript
				signingService,                    // the signing service URI embedded in the Javascript
				docID,                             // DocumentID: we are only including it for informational purposes here
				preSignedFile,                     // A path to the presigned document (again, for informational purposes)
				asyncReqStr.c_str(),               // a copy of async request to be shown on the page (HTMLEncoded for safety)
				signingService,                    // the signing service URI again (for informational purposes, to be shown on the page)
				completionEndpoint                 // the completion URI in the form's "action" parameter.
			);

			this->SetResponseStatus(connectionID, 200);
			this->SetResponseString(connectionID, body, "text/html");
		}
		else
		{
			this->Config("RequestFilter=");

			printf("Error: [%i] %s\n", this->GetLastErrorCode(), this->GetLastError());
			errorPage(connectionID, this->GetLastErrorCode(), this->GetLastError());
		}
	}

	char * ReadAllBytes(const char * filename, int * read)
	{
		std::ifstream ifs(filename, std::ios::binary | std::ios::ate);
		std::ifstream::pos_type pos = ifs.tellg();

		// What happens if the OS supports really big files.
		// It may be larger than 32 bits?
		// This will silently truncate the value/
		int length = pos;

		// Manuall memory management.
		// Not a good idea use a container/.
		char *pChars = new char[length];
		ifs.seekg(0, std::ios::beg);
		ifs.read(pChars, length);

		// No need to manually close.
		// When the stream goes out of scope it will close the file
		// automatically. Unless you are checking the close for errors
		// let the destructor do it.
		ifs.close();
		*read = length;
		return pChars;
	}

	// Outputs the DCAuth Javascript file.
	void flushDCAuthJS(int64 connectionID)
	{
		// WARNING! THIS IS A DUMMY PLACEHOLDER. THIS NEEDS TO BE UPDATED TO LOAD
		// THE JAVASCRIPT FILE CAREFULLY.

		printf("DCAuth Javascript requested.");

		int bufSize;
		char* buf = ReadAllBytes(dcAuthJSPath, &bufSize);

		this->SetResponseStatus(connectionID, 200);
		this->SetResponseBytes(connectionID, buf, bufSize, "text/javascript");
	}

	// Implements the completion logic. Takes an async response on input,
	// collects the pre-signed copy from a local directory, embeds the signature,
	// and saves the resulting document in another local directory.
	char* completeSigning(char* asyncResponse, int64 connectionID)
	{
		// This method completes the signing by inserting the signature into the earlier pre-signed document.

		// Specifically, the SignAsyncEnd call performs the following steps:
		//	- extracts the signature from the async response,
		//	- validates its integrity,
		//	- embeds the signature, along with any other necessary elements, to the signature,
		//	- if the TSA URL was provided, timestamps the created signature.

		PDFSigner signer;

		if (useJSMode)
			signer.SetExternalCryptoMode(ECM_DCAUTHJSON);
		else
			signer.SetExternalCryptoMode(ECM_DCAUTH);

		// Extracting the DocID from the 'user data' mirrored by the DCAuth
		// service in its async response.
		char* docID = signer.ExtractAsyncData(asyncResponse);

		// Checking if a pre-signed file with such DocID exists.
		char* preSignedFile = new char[MAX_PATH];
		sprintf(preSignedFile, "%s\\%s", preSignedDocFolder, docID);
		if (!fileExists(preSignedFile))
			throw std::runtime_error("Pre-signed file not found");

		// Loading the pre-signed document.
		FILE* file = fopen(preSignedFile, "rb");
		fseek(file, 0, SEEK_END);
		int length = ftell(file);
		fseek(file, 0, SEEK_SET);
		char* preSignedDoc = new char[length];
		fread(preSignedDoc, 1, length, file);
		fclose(file);

		// Assigning the pre-signed document
		signer.SetInputBytes(preSignedDoc, length);

		// Cancelling any chain validation.
		signer.SetIgnoreChainValidationErrors(true);

		// Assigning credentials.
		signer.SetExternalCryptoKeyID(keyID);
		signer.SetExternalCryptoKeySecret(keySecret);

		// Assigning the timestamping service URL. This time the signer object
		// makes real use of it.
		signer.SetTimestampServer(TSAURL);

		// Completing the signing.
		signer.SignAsyncEnd(asyncResponse);

		if (signer.GetLastErrorCode() == 0)
		{
			// Saving the now fully signed document to a local directory.
			int outBufSize;
			char* outBuf;
			signer.GetOutputBytes(outBuf, outBufSize);

			char* signedFile = new char[MAX_PATH];
			sprintf(signedFile, "%s\\%s.pdf", signedDocFolder, docID);

			FILE* file = fopen(signedFile, "wb");
			fwrite(outBuf, 1, outBufSize, file);
			fclose(file);

			return docID;
		}
		else
		{
			errorPage(connectionID, signer.GetLastErrorCode(), signer.GetLastError());
			return "";
		}
	}

	// Completes the signature and saves a fully signed document. This is produced
	// in response to a Javascript POST containing the async response from the pre-signing page.
	void completionPage(int64 connectionID)
	{
		char* pageTpl =
			"<html>\r\n"
			"<head><title>DCAuth demo: signing completed</title></head>\r\n"
			"<body style=\"font-family: sans-serif\">\r\n"
			"  <h1>DCAuth demo: the web application</h1>\r\n"
			"  <h2>Signing completed successfully</h2>\r\n"
			"  The remote signing procedure for your document has completed successfully. \r\n"
			"  Click <a href=\"%s\" target=\"_blank\">here</a> to download the signed document. Click <a href=\"/\">here</a> to start again.\r\n"
			"</body>\r\n"
			"</html>";

		printf("The completion page handler starting.\n");

		// the async response is provided in a simple URLEncoded form
		this->Config("RequestFilter=parts[0]");

		char* resp = this->GetRequestString(connectionID);

		if (this->GetLastErrorCode() == 0)
		{
			this->Config("RequestFilter=");

			// We are using a very simple param provision procedure here not to overcomplicate things.
			// In a real-world app you can use as complex form as you like, including
			// other parameters alongside the async response.
			if (strncmp(resp, "asyncresp=", 10) == 0)
			{
				std::string substr{ resp + 10, strlen(resp) - 10 };
				strcpy(resp, substr.c_str());
			}
			else
				throw std::runtime_error("Unexpected request: asyncresp parameter expected");

			printf("Received async response(%d bytes). Completing the signing.\n", strlen(resp));

			// Completing the signing. This call return us a DocID so we could form a download link.
			char* docID = completeSigning(resp, connectionID);
			if (strcmp(docID, "") == 0)
				return;

			printf("Signing completed for DocID: %s\n", docID);
			printf("Producing the page with a link to the signed document.\n");

			char* body = new char[2000];
			char* getFileURL = new char[MAX_PATH];
			sprintf(getFileURL, "%s?id=%s", getFileEndpoint, docID);
			sprintf(body, pageTpl, getFileURL);
			
			this->SetResponseStatus(connectionID, 200);
			this->SetResponseString(connectionID, body, "text/html");
		}
		else
		{
			this->Config("RequestFilter=");

			printf("Error: [%i] %s\n", this->GetLastErrorCode(), this->GetLastError());
			errorPage(connectionID, this->GetLastErrorCode(), this->GetLastError());
		}
	}

	// Outputs welcome/information page. It is only used for debug/testing purposes,
	// a live signing service does not need it.
	void welcomePage(int64 connectionID)
	{
		char* pageTpl =
			"<html>\r\n"
			"<head><title>DCAuth demo: welcome</title></head>\r\n"
			"<body style=\"font-family: sans-serif\">\r\n"
			"  <h1>DCAuth demo: the web application</h1>\r\n"
			"  <h2>Welcome to DCAuth demo</h2>\r\n"
			"  When you click the Start button, a chosen PDF document will be uploaded\r\n"
			"  (POST'ed) to the pre-signing page. During the generation of the pre-signing\r\n"
			"  page the following will happen: \r\n"
			"  <ul>\r\n"
			"    <li>A document hash will be calculated and incorporated into an &quot;async request&quot;,\r\n"
			"    <li>The async request will be incorporated into the body of the produced page, \r\n"
			"    <li>A pre-signed copy of the document will be saved to the special directory on the web server.\r\n"
			"  </ul>\r\n"
			"  When your browser receives the output of the pre-signing page,\r\n"
			"  it will run the Javascript embedded into the page to pass the async request\r\n"
			"  to the local signing service and ask it to sign the hash.\r\n"
			"  This web application is configured to assume that the signing\r\n"
			"  service is accessible from the browser at the following location: %s. \r\n"
			"  Please make sure the signing service is active before proceeding to the \r\n"
			"  pre-signing stage.\r\n"
			"  <p>\r\n"
			"  If you are using the second part of the DCAuth demo as your signing service,\r\n"
			"  you can check its availability by following <a href=\"%s\" target=\"_blank\">this link</a>\r\n"
			"  (the service should respond with a welcome/status page)\r\n"
			"  <form action=\"%s\" method=\"post\" enctype=\"multipart/form-data\">\r\n"
			"    Please choose a PDF file to sign: <input type=\"file\" name=\"pdffile\" /><br />\r\n"
			"    <input type=\"submit\" value=\"Start signing\" />\r\n"
			"  </form>\r\n"
			"</body>\r\n"
			"</html>";

		printf("Producing the welcome page\n");

		// Forming the page on the basis of a template
		char* body = new char[2000];
		sprintf(body, pageTpl,
			signingServiceURL, // the value included on the page
			signingServiceURL, // the link target
			preSigningEndpoint // the form action
		);

		this->SetResponseStatus(connectionID, 200);
		this->SetResponseString(connectionID, body, "text/html");

		if (this->GetLastErrorCode() != 0)
		{
			printf("Error: [%i] %s\n", this->GetLastErrorCode(), this->GetLastError());
			errorPage(connectionID, this->GetLastErrorCode(), this->GetLastError());
		}
	}

	// Just flushes the signed PDF file.
	void flushFile(int64 connectionID)
	{
		printf("Flushing a file\n");

		// The id of the file is provided as the 'id' parameter
		this->Config("RequestFilter=params[0]");

		char* docID = this->GetRequestString(connectionID);

		if (this->GetLastErrorCode() == 0)
		{
			this->Config("RequestFilter=");

			if (strncmp(docID, "id=", 3) == 0)
			{
				std::string substr{ docID + 3, strlen(docID) - 3 };
				strcpy(docID, substr.c_str());
			}
			else
				throw std::runtime_error("Unexpected request: id parameter expected.");

			printf("File requested: %s\n", docID);

			char* signedFile = new char[MAX_PATH];
			sprintf(signedFile, "%s\\%s.pdf", signedDocFolder, docID);
			if (!fileExists(signedFile))
				throw std::runtime_error("Signed file with this ID has not been found");

			// Reading the file contents
			FILE* file = fopen(signedFile, "rb");
			fseek(file, 0, SEEK_END);
			int length = ftell(file);
			fseek(file, 0, SEEK_SET);
			char* data = new char[length];
			fread(data, 1, length, file);
			fclose(file);

			// Flushing the PDF file out
			this->SetResponseStatus(connectionID, 200);
			this->SetResponseBytes(connectionID, data, length, "application/pdf");
		}
		else
		{
			this->Config("RequestFilter=");

			printf("Error: [%i] %s\n", this->GetLastErrorCode(), this->GetLastError());
			errorPage(connectionID, this->GetLastErrorCode(), this->GetLastError());
		}
	}

public:

	virtual int FireGetRequest(HTTPServerGetRequestEventParams* e)
	{
		// This event handler fires when the web application receives a GET request.
		// This web app can respond to two kinds of GET requests: to /getfile resource,
		// which makes it flush a signed PDF document with the requested ID,
		// or to any other resource, which makes it produce a welcome page.

		printf("Received GET for %s from connection %d \n", e->URI, e->ConnectionID);

		if (strncmp(e->URI, "/getfile", 8) == 0) // a special URI for the signed file
		{
			flushFile(e->ConnectionID);
		}
		else if (strncmp(e->URI, preSigningJSEndpoint, 8) == 0) // This endpoint is triggered when the browser requests a DCAuth javascript referenced from the javascript presigning page
			flushDCAuthJS(e->ConnectionID);
		else
		{
			// Whatever other page is requested, outputting the welcome page
			welcomePage(e->ConnectionID);
		}

		e->Handled = true;

		return 0;
	}

	virtual int FirePostRequest(HTTPServerPostRequestEventParams* e)
	{
		// This event handler fires when the web application receives a POST request.
		// The application can process POST requests to the following endpoints:
		// - the presigning endpoint (receives unsigned PDF documents, submitted by the users from the welcome page)
		// - the completion endpoint (receives async responses submitted by the Javascript from the pre-signed page)
		// - anything else, in case of which the welcome page is displayed.

		printf("Received POST for %s from connection %i \n", e->URI, e->ConnectionID);

		if (strcmp(e->URI, preSigningEndpoint) == 0)
		{
			if (!useJSMode) // the pre-signing page for the "default" DCAuth signing
				preSigningPage(e->ConnectionID);
			else // the pre-signing page for javascript signing
				preSigningPageJSMode(e->ConnectionID);
		}
		else if (strcmp(e->URI, completionEndpoint) == 0)
		{
			completionPage(e->ConnectionID);
		}
		else
		{
			welcomePage(e->ConnectionID);
		}

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
	
	char* appPath = new char[MAX_PATH];
	GetCurrentDirectory(MAX_PATH, appPath);

	// The endpoints (relative to the local host)
	if (optext(argc, argv, "-presignendpoint"))
	{
		preSigningEndpoint = optval(argc, argv, "-presignendpoint");

		preSigningJSEndpoint = new char[strlen(preSigningEndpoint) + strlen("/js") + 1];
		strcpy(preSigningJSEndpoint, preSigningEndpoint);
		strcat(preSigningJSEndpoint, "/js");
	}
	if (optext(argc, argv, "-completendpoint"))
	{
		completionEndpoint = optval(argc, argv, "-completendpoint");
	}

	// Authentication credentials: should match those used by the DCAuth signing service
	if (optext(argc, argv, "-keyid"))
	{
		keyID = optval(argc, argv, "-keyid");
	}
	if (optext(argc, argv, "-keysecret"))
	{
		keySecret = optval(argc, argv, "-keysecret");
	}

	// Signing service URL: make sure to specify it from the perspective
	// of the systems where the web app will run, not this web app system.
	// This URL is defined by the configuration of the second part of the sample
	// (the DCAuth signing service), which has the default setting of http://127.0.0.1:17080/sign.
	if (optext(argc, argv, "-signurl"))
	{
		signingServiceURL = optval(argc, argv, "-signurl");
	}

	// The DCAuth method: PKCS1 or PKCS7
	if (optext(argc, argv, "-usepkcs7"))
	{
		usePKCS7Method = true;
	}

	// In the Javascript mode, the DCAuth states are processed in the web browser by the DCAuth Javascript module
	// (included with this sample), and only plain hashes are submitted to the DCAuthService listener.
	if (optext(argc, argv, "-usejs"))
	{
		useJSMode = true;

		dcAuthJSPath = new char[MAX_PATH];
		sprintf(dcAuthJSPath, "%s\\dcauth.js", appPath);

		// We can't use java script mode without dcauth.js file
		if ((useJSMode) && (access(dcAuthJSPath, 0) == -1))
		{
			useJSMode = false;
			printf("A \"dcauth.js\" is required for java script mode. The mode has been switched to default.\n");
		}
	}
	
	if (optext(argc, argv, "-cert"))
	{
		certfile = optval(argc, argv, "-cert");
	}
	else
	{
		// default cert file path
		certfile = new char[MAX_PATH];
		sprintf(certfile, "%s\\cert.cer", appPath);
	}


	// The timestamping service URI
	if (optext(argc, argv, "-tsaurl"))
	{
		TSAURL = optval(argc, argv, "-tsaurl");
	}

	// Local folders to keep the presigned and signed documents in
	if (optext(argc, argv, "-presign"))
	{
		preSignedDocFolder = optval(argc, argv, "-presign");
	}
	else
	{
		// default preSigned document path
		char* tempPath = new char[MAX_PATH];
		GetTempPathA(MAX_PATH, tempPath);
		preSignedDocFolder = new char[strlen(tempPath) + strlen("presigned") + 1];
		strcpy(preSignedDocFolder, tempPath);
		strcat(preSignedDocFolder, "presigned");
	}
	CreateDirectory(preSignedDocFolder, NULL);

	if (optext(argc, argv, "-sign"))
	{
		signedDocFolder = optval(argc, argv, "-sign");
	}
	else
	{
		// default preSigned document path
		char* tempPath = new char[MAX_PATH];
		GetTempPathA(MAX_PATH, tempPath);
		signedDocFolder = new char[strlen(tempPath) + strlen("signed") + 1];
		strcpy(signedDocFolder, tempPath);
		strcat(signedDocFolder, "signed");
	}
	CreateDirectory(signedDocFolder, NULL);

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
		webserver.SetPort(16080);
	}

	// No need for keep-alive connections
	webserver.SetUseTLS(false);

	// Starting the server
	res = webserver.Start();
	if (res == 0)
	{
		printf("\nThe web server has started on port %d. Press enter to stop and exit.\n\n", webserver.GetPort());
		getchar();

		webserver.Stop();
		printf("The web server has stopped\n");

		return 0;
	}
	else
	{
		printf("Failed to run the server, error %d\n", res);
		getchar();

		return 2;
	}
}





