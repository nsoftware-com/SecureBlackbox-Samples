/*
 * SecureBlackbox 2022 Java Edition - Sample Project
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

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import secureblackbox.*;

public class dcauthwebserver extends ConsoleDemo
{
	// The web server that listens to relayed async requests from the browser.
	static Httpserver webserver = new Httpserver();

	// The certificate manager to load the public certificate
	static Certificatemanager certmanager = new Certificatemanager();

	// The web server endpoints: the pre-signing one, the completion one, and the one that flushes the signed files.
	static String preSigningEndpoint = "/start";
	static String completionEndpoint = "/finish";
	static String getFileEndpoint = "/getfile";

	// The URI of the signing service. This will be embedded into the web page,
	// and used by the Javascript to contact the service. Make sure this matches
	// the one configured in the signing service piece.
	static String signingServiceURL = "http://127.0.0.1:17080/sign";

	// A local (to the web app) directory to keep the pre-signed files.
	static String preSignedDocFolder = "";

	// A local (to the web app) directory to keep the signed files.
	static String signedDocFolder = "";

	// Whether to use PKCS7 (true) or PKCS1 (false) DCAuth method.
	static boolean usePKCS7Method = false;

	// Whether to use Javascript.
	static boolean useJSMode = false;

	// Path of dcauth java script
	static String dcAuthJSPath = "";

	// The DCAuth authentication credentials: KeyID and KeySecret.
	// These two should match the ones used by the signing service.
	static String keyID = "mykeyid";
	static String keySecret = "mykeysecret";

	// The URI of the timestamping service.
	static String TSAURL = "";

	public static final String CrLf = "\r\n";
	public static final String PKCS1Format = "?format=pkcs1";
	public static final String PKCS7Format = "?format=pkcs7";

	public static Certificate loadCertificate(String file, String password)
	{
		Certificate cert = null;

		if (file.length() > 0)
		{
			try
			{
				Certificatemanager certmanager = new Certificatemanager();

				certmanager.importFromFile(file, password);

				cert = certmanager.getCertificate();
			}
			catch (Exception e)
			{
				System.out.println("Cannot load certificate!");
			}
		}

		return cert;
	}

	public static int parseWithDefault(String s, int defaultVal) {
		return s.matches("-?\\d+") ? Integer.parseInt(s) : defaultVal;
	}

	private static String htmlEncode(String s, boolean encodeSpecialChars) {
		s = noNull(s, "");

		StringBuilder str = new StringBuilder();

		for (int j = 0; j < s.length(); j++) {
			char c = s.charAt(j);

			// encode standard ASCII characters into HTML entities where needed
			if (c < '\200') {
				switch (c) {
					case '"':
						str.append("&quot;");

						break;

					case '&':
						str.append("&amp;");

						break;

					case '<':
						str.append("&lt;");

						break;

					case '>':
						str.append("&gt;");

						break;

					default:
						str.append(c);
				}
			}
			// encode 'ugly' characters (ie Word "curvy" quotes etc)
			else if (encodeSpecialChars && (c < '\377')) {
				String hexChars = "0123456789ABCDEF";
				int a = c % 16;
				int b = (c - a) / 16;
				str.append("&#x").append(hexChars.charAt(b)).append(hexChars.charAt(a)).append(';');
			}
			//add other characters back in - to handle charactersets
			//other than ascii
			else {
				str.append(c);
			}
		}

		return str.toString();
	}

	private static String noNull(String string, String defaultString) {
		return (stringSet(string)) ? string : defaultString;
	}

	private static boolean stringSet(String string) {
		return (string != null) && !"".equals(string);
	}

	public static void main(String[] args) {
		if (args.length < 1) {
			System.out.println("usage: dcauthwebserver <listening_port> [-presignendpoint presigning_endpoint] [-completendpoint completion_endpoint] ");
			System.out.println("   [-keyid key_id] [-keysecret key_secret] [-signurl signing_service_url] [-usepkcs7] [-usejs]");
			System.out.println("   [-cert certificate_file] [-tsaurl timestamp_service_url] [-presign pre_signed_doc_folder] [-sign signed_doc_folder]");
			System.out.println("Options: ");
			System.out.println("  -presignendpoint     The pre-signing endpoint.");
			System.out.println("  -completendpoint     The completion endpoint.");
			System.out.println("Enter the port number that you want your web server to listen on, and the web endpoints for initiation and completion stages of the async signing workflow.");
			System.out.println("  -keyid        The key ID.");
			System.out.println("  -keysecret    The key Secret.");
			System.out.println("  -signurl      The signing service URL on the connecting systems.");
			System.out.println("The Key ID and Key Secret should match those used by the signing component on the pre-signing stage");
			System.out.println("  -usepkcs7     Whether to use PKCS7 DCAuth method.");
			System.out.println("  -usejs        Whether to use java script mode.");
			System.out.println("  -cert         The certificate used to sign file.");
			System.out.println("The PKCS#1 method requires the public part of the signing certificate to be available for the web app");
			System.out.println("  -tsaurl       The timestamp service URL.");
			System.out.println("  -presign      The pre-signed documents folder.");
			System.out.println("  -sign         The signed documents folder.");
			System.out.println("\r\nExample: dcauthwebserver 16080");
			System.out.println("             dcauthwebserver 16080 -presignendpoint /start -completendpoint /finish -keyid mykeyid -keysecret mykeysecret -signurl http://127.0.0.1:17080/sign");
			System.out.println("                 -usepkcs7 -cert C:\\\\certs\\\\mycert.pfx -tsaurl mytimestampserver -presign C:\\\\presignfolder\\\\ -sign C:\\\\signfolder\\\\");
		}
		else
		{
			try
			{
				System.out.println("***************************************************************************************************");
				System.out.println("This demo application is a part of SecureBlackbox remote signing (DCAuth) demo. It runs a web app ");
				System.out.println("that can work with the DCAuth signing service (the second part of the demo) to sign PDF documents remotely.");
				System.out.println("");
				System.out.println("To evaluate the demo, please do the following: ");
				System.out.println("");
				System.out.println("    1) Run this application, set up the parameters of the web app (if needed), and start the web app.");
				System.out.println("    2) Run the second part of the demo (the signing service) on this or a different system.");
				System.out.println("    3) On the \"signing service\" system, navigate to this web application in the browser. In default configuration, and when");
				System.out.println("       running both parts of the demo on the same system, it can be accessed at http://127.0.0.1:16080.");
				System.out.println("    4) Follow the guidance of the web app.");
				System.out.println("***************************************************************************************************\n");


				// HTTP request handlers: we want to intercept GETs and POSTs
				webserver.addHttpserverEventListener(new HttpserverEventListener() {
					@Override
					public void accept(HttpserverAcceptEvent e) {

					}

					@Override
					public void authAttempt(HttpserverAuthAttemptEvent e) {

					}

					@Override
					public void certificateValidate(HttpserverCertificateValidateEvent e) {

					}

					@Override
					public void connect(HttpserverConnectEvent e) {

					}

					@Override
					public void customRequest(HttpserverCustomRequestEvent e) {

					}

					@Override
					public void data(HttpserverDataEvent e) {

					}

					@Override
					public void deleteRequest(HttpserverDeleteRequestEvent e) {

					}

					@Override
					public void disconnect(HttpserverDisconnectEvent e) {

					}

					@Override
					public void error(HttpserverErrorEvent e) {

					}

					@Override
					public void externalSign(HttpserverExternalSignEvent e) {

					}

					@Override
					public void fileError(HttpserverFileErrorEvent e) {

					}

					@Override
					public void getRequest(HttpserverGetRequestEvent e) {
						// This event handler fires when the web application receives a GET request.
						// This web app can respond to two kinds of GET requests: to /getfile resource,
						// which makes it flush a signed PDF document with the requested ID,
						// or to any other resource, which makes it produce a welcome page.

						log("Received GET from connection " + e.connectionID + " for " + e.URI);

						if (e.URI.startsWith("/getfile")) // a special URI for the signed file
							flushFile(e.connectionID);
						else if (e.URI.equals(preSigningEndpoint + "/js")) // This endpoint is triggered when the browser requests a DCAuth javascript referenced from the javascript presigning page
							flushDCAuthJS(e.connectionID);
  						else
							// Whatever other page is requested, outputting the welcome page
							welcomePage(e.connectionID);

						e.handled = true;
					}

					@Override
					public void headRequest(HttpserverHeadRequestEvent e) {

					}

					@Override
					public void notification(HttpserverNotificationEvent e) {

					}

					@Override
					public void optionsRequest(HttpserverOptionsRequestEvent e) {

					}

					@Override
					public void patchRequest(HttpserverPatchRequestEvent e) {

					}

					@Override
					public void postRequest(HttpserverPostRequestEvent e) {
						// This event handler fires when the web application receives a POST request.
						// The application can process POST requests to the following endpoints:
						// - the presigning endpoint (receives unsigned PDF documents, submitted by the users from the welcome page)
						// - the completion endpoint (receives async responses submitted by the Javascript from the pre-signed page)
						// - anything else, in case of which the welcome page is displayed.

						log("Received POST from connection " + e.connectionID + " for " + e.URI);

						if (e.URI.equals(preSigningEndpoint))
						{
							if (!useJSMode) // the pre-signing page for the "default" DCAuth signing
								preSigningPage(e.connectionID);
							else // the pre-signing page for javascript signing
								preSigningPageJSMode(e.connectionID);
						}
  						else if (e.URI.equals(completionEndpoint))
							completionPage(e.connectionID);
  						else
							welcomePage(e.connectionID);

						e.handled = true;
					}

					@Override
					public void putRequest(HttpserverPutRequestEvent e) {

					}

					@Override
					public void TLSEstablished(HttpserverTLSEstablishedEvent e) {

					}

					@Override
					public void TLSPSK(HttpserverTLSPSKEvent e) {

					}

					@Override
					public void TLSShutdown(HttpserverTLSShutdownEvent e) {

					}

					@Override
					public void traceRequest(HttpserverTraceRequestEvent e) {

					}

					@Override
					public void supercoreIntercept(HttpserverSupercoreInterceptEvent e) {

					}
				});

				String certfile = new File(".").getCanonicalPath() + "\\cert.cer";
				dcAuthJSPath = new File(".").getCanonicalPath() + "\\dcauth.js";
				preSignedDocFolder = System.getProperty("java.io.tmpdir") + "presigned";
				signedDocFolder = System.getProperty("java.io.tmpdir") + "signed";

				for (int i = 1; i < args.length; i++)
				{
					if (args[i].startsWith("-"))
					{
						// The endpoints (relative to the local host)
						if (args[i].equals("-presignendpoint"))
							preSigningEndpoint = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-completendpoint"))
							completionEndpoint = args[i + 1]; // args[i+1] corresponds to the value of argument [i]

						// Authentication credentials: should match those used by the DCAuth signing service
						if (args[i].equals("-keyid"))
							keyID = args[i + 1];
						if (args[i].equals("-keysecret"))
							keySecret = args[i + 1];

						// Signing service URL: make sure to specify it from the perspective
						// of the systems where the web app will run, not this web app system.
						// This URL is defined by the configuration of the second part of the sample
						// (the DCAuth signing service), which has the default setting of http://127.0.0.1:17080/sign.
						if (args[i].equals("-signurl"))
							signingServiceURL = args[i + 1];

						// The DCAuth method: PKCS1 or PKCS7
						if (args[i].equals("-usepkcs7"))
							usePKCS7Method = true;

						// In the Javascript mode, the DCAuth states are processed in the web browser by the DCAuth Javascript module
						// (included with this sample), and only plain hashes are submitted to the DCAuthService listener.
						if (args[i].equals("-usejs"))
							useJSMode = true;

						if (args[i].equals("-cert"))
							certfile = args[i + 1];

						// The timestamping service URI
						if (args[i].equals("-tsaurl"))
							TSAURL = args[i + 1];

						// Local folders to keep the presigned and signed documents in
						if (args[i].equals("-presign"))
							preSignedDocFolder = args[i + 1];
						if (args[i].equals("-sign"))
							signedDocFolder = args[i + 1];
					}
				}

				File preSignedDocDir = new File(preSignedDocFolder);
				if (! preSignedDocDir.exists())
				{
					preSignedDocDir.mkdir();
				}

				File signedDocDir = new File(signedDocFolder);
				if (! signedDocDir.exists())
				{
					signedDocDir.mkdir();
				}

				// Loading the certificate. The certificate is mandatory if PKCS#1 method
				// is used. It may only be needed with PKCS#7 method if you want to fill
				// the signature widget texts basing on the information contained in it.
				certmanager.setCertificate(null);
				if (new File(certfile).exists())
				{
					certmanager.setCertificate(loadCertificate(certfile, ""));
				}

				// We can't use the PKCS#1 method if there is no public certificate on the
				// web application's side, so switching to PKCS#7 if the certificate is absent.
				if ((certmanager.getCertificate() == null) && (!usePKCS7Method))
				{
					usePKCS7Method = true;
					System.out.println("A certificate is required for DCAuth PKCS#1 method to work. The method has been switched to PKCS#7.");
				}

				// We can't use java script mode without dcauth.js file
				if ((useJSMode ) && (!(new File(dcAuthJSPath).exists())))
				{
					useJSMode = false;
					System.out.println("A \"dcauth.js\" is required for java script mode. The mode has been switched to default.");
				}

				//== Preparing the web server

				// Port to listen on
				webserver.setPort(parseWithDefault(args[0], 16080));

				// No TLS in this simple demo
				webserver.setUseTLS(false);

				// Starting the server
				webserver.start();

				System.out.println("");
				System.out.println("The web server has started on port " + webserver.getPort() + ". Press enter to stop and exit.");
				System.out.println("");
				System.in.read();

				webserver.stop();
				System.out.println("The web server has stopped");
			}
			catch (Exception ex)
			{
				displayError(ex);
			}
		}
	}

	public static void log(String mes)
	{
		System.out.println(mes);
	}

	// Pre-signs the document and outputs a web page that contains the async request
	// and the Javascript that can relay it to the signing service. This is produced
	// in response to a POST from the welcome page containing the unsigned document.
	public static void preSigningPage(long connectionID)
	{
		String pageTpl =
			"<html>" + CrLf +
			"<head>" + CrLf +
			"  <title>DCAuth demo: pre-signing completed</title>" + CrLf +
			"  <script>" + CrLf +
			"    // The async request itself." + CrLf +
			"    var asyncReq = \"%s\"; " + CrLf +
			"" + CrLf +
			"    // The URL where to submit the async response to." + CrLf +
			"    var completionUri = \"%s\"; " + CrLf +
			"" + CrLf +
			"    // This function relays the async request to the DCAuth signing service." + CrLf +
			"    function submitAsyncRequest() {" + CrLf +
			"      var xhr = new XMLHttpRequest();" + CrLf +
			"      xhr.open(\"POST\", \"%s\", true);" + CrLf +
			"      xhr.setRequestHeader(\"Content-Type\", \"application/x-www-form-urlencoded\");" + CrLf +
			"      xhr.onload = function() {" + CrLf +
			"        if (this.status != 200) {" + CrLf +
			"          alert(\"Something went wrong: \" + this.response);" + CrLf +
			"        } else {" + CrLf +
			"          // Saving a copy of the response in a page element for debug purposes." + CrLf +
			"          var respElem = document.getElementById(\"asyncresptext\"); " + CrLf +
			"          respElem.innerHTML = \"<p>The below async response was received: " +
			"          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + this.response.replace(/&/g, \"&amp;\").replace(/</g, \"&lt;\").replace(/>/g, \"&gt;\").replace(/'/g, \"&#39;\").replace(/\"/g, \"&#34;\") + \"</textarea>\";" + CrLf +
			"" + CrLf +
			"          // Reporting the success of the operation and asking the user whether we can proceed." + CrLf +
			"          var res = confirm(\"A successful async response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this response back to the web application at \" + completionUri + \".\");" + CrLf +
			"          if (res == true) {" + CrLf +
			"            // Submitting the form to the web application's completion endpoint." + CrLf +
			"            document.getElementById(\"asyncresp\").value = this.response;" + CrLf +
			"            document.getElementById(\"respform\").submit();" + CrLf +
			"          }" + CrLf +
			"        }" + CrLf +
			"      }" + CrLf +
			"      xhr.send(asyncReq);" + CrLf +
			"    }" + CrLf +
			"  </script>" + CrLf +
			"</head>" + CrLf +
			"<body style=\"font-family: sans-serif\">" + CrLf +
			"  <h1>DCAuth demo: the web application</h1>" + CrLf +
			"  <h2>Pre-signing completed. </h2>" + CrLf +
			"  The PDFSigner component has successfully pre-signed the provided PDF document." + CrLf +
			"  The pre-signed document has been assigned with a DocID of %s " + CrLf +
			"  and saved to the following location: %s." + CrLf +
			"  <p>" + CrLf +
			"  The async request shown below has been generated as a result of the pre-signing. " + CrLf +
			"  This has also been included in the body of this page, and will be " + CrLf +
			"  submitted to your local signing endpoint by the embedded Javascript when you click the Sign" + CrLf +
			"  button below. The signing procedure is performed behind the scenes and " + CrLf +
			"  is fully transparent for you." + CrLf +
			"  <p>" + CrLf +
			"  <textarea cols=\"50\" rows=\"8\">%s</textarea>." + CrLf +
			"  <p>" + CrLf +
			"  This page expects the signing endpoint to be accessible at %s." + CrLf +
			"  If the endpoint cannot be reached at this address, the signing will fail." + CrLf +
			"  <p>" + CrLf +
			"  If the signing completes successfully and a good async response containing" + CrLf +
			"  the signature has been received from the signing service, this page will submit " + CrLf +
			"  it back to the web app for completion of the signing operation. The page " + CrLf +
			"  will notify you when this happens. " + CrLf +
			"  <p>Now click the Sign button to pass the async request to the signing service: " + CrLf +
			"  <button onclick=\"submitAsyncRequest()\">Sign</button>" + CrLf +
			"  <p><div id=\"asyncresptext\"></div>" + CrLf +
			"  <form action=\"%s\" method=\"post\" id=\"respform\">" + CrLf +
			"    <input type=\"hidden\" value=\"\" id=\"asyncresp\" name=\"asyncresp\" />" + CrLf +
			"  </form>" + CrLf +
			"</body>" + CrLf +
			"</html>";

		log("The pre-signing page handler starting.");

		byte[] buf;
		try
		{
			// Recovering the PDF document from the POST, which is passed as a part of a multipart HTTP request
			try
			{
				webserver.config("RequestFilter=parts[0]");
				buf = webserver.getRequestBytes(connectionID);
			}
			finally
			{
				webserver.config("RequestFilter=");
			}

			log("Received a document (" + buf.length + " bytes). Initiating the pre-signing.");

			// Generating a unique document ID.
			String docID = "";
			do
				docID = Long.toHexString(System.currentTimeMillis());
			while (new File(preSignedDocFolder + "\\" + docID).exists());

			// Passing the document for pre-signing. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			String asyncReq = preSignDocument(buf, docID, connectionID);
			if (asyncReq == "")
				return;

			log("Pre-signing completed; DocID is " + docID + ", async request is " + asyncReq.length() + " bytes long.");
			log("Producing the page with embedded async request.");

			// Forming the page by inserting a variety of variable pieces to the template.
			// Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
			// the page's syntactical safety. The signing service URLDecodes it.
			String body = String.format(pageTpl,
					URLEncoder.encode(asyncReq, StandardCharsets.UTF_8.name()),  // async request body embedded in the Javascript.
					completionEndpoint,                          // completion URI embedded in the Javascript
					signingServiceURL,                           // the signing service URI embedded in the Javascript
					docID,                                       // DocumentID: we are only including it for informational purposes here
					preSignedDocFolder + "\\" + docID,           // A path to the presigned document (again, for informational purposes)
					htmlEncode(asyncReq, true), // a copy of async request to be shown on the page (HTMLEncoded for safety)
					signingServiceURL,                           // the signing service URI again (for informational purposes, to be shown on the page)
					completionEndpoint                           // the completion URI in the form's "action" parameter.
    		);

			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseString(connectionID, body, "text/html");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
		}
	}

	// Pre-signs the document and outputs a web page that contains the async request in JSON format.
	public static void preSigningPageJSMode(long connectionID) {
		String PageTpl =
			"<html>" + CrLf +
			"<head>" + CrLf +
			"  <title>DCAuth demo: pre-signing completed</title>" + CrLf +
			"  <script type=\"text/javascript\" src=\"%s/js\" ></script>" + CrLf +
			"  <script>" + CrLf +
			"    // The async request itself." + CrLf +
			"    var asyncReq = %s; " + CrLf +
			"" + CrLf +
			"    // The URL where to submit the async response to." + CrLf +
			"    var completionUri = \"%s\"; " + CrLf +
			"" + CrLf +
			"    try" + CrLf +
			"    {" + CrLf +
			"      var request = new DcauthAsyncRequest(asyncReq);" + CrLf +
			"    }" + CrLf +
			"    catch(err) {" + CrLf +
			"      alert(\"Something went wrong: \" + err.message);" + CrLf +
			"    }" + CrLf +
			"    var hash = request.getHash();" + CrLf +
			"" + CrLf +
			"    // This function relays the hash to the DCAuth signing service." + CrLf +
			"    function submitAsyncRequest() {" + CrLf +
			"      var xhr = new XMLHttpRequest();" + CrLf +
			"      xhr.open(\"POST\", \"%s\", true);" + CrLf +
			"      xhr.setRequestHeader(\"Content-Type\", \"application/x-www-form-urlencoded\");" + CrLf +
			"      xhr.onload = function() {" + CrLf +
			"        if (this.status != 200) {" + CrLf +
			"          alert(\"Something went wrong: \" + this.response);" + CrLf +
			"        } else {" + CrLf +
			"          // Forming the response state basing on the javascript signature we received and saving it in a page element for debug purposes." + CrLf +
			"          var response = new DcauthAsyncResponse(request);" + CrLf +
			"          response.setSignature(this.response);" + CrLf +
			"          var respElem = document.getElementById(\"asyncresptext\"); " + CrLf +
			"          respElem.innerHTML = \"<p>The below async state was composed from the signature received from the service: " +
			"          <p><textarea cols=\\\"50\\\" rows=\\\"8\\\">\" + response.savetostring(); + \"</textarea>\";" + CrLf +
			"" + CrLf +
			"          // Reporting the success of the operation and asking the user whether we can proceed." + CrLf +
			"          var res = confirm(\"A successful response has been received from the signing endpoint (and a copy has been added to the page for your records). Press OK to submit this signature back to the web application at \" + completionUri + \".\");" + CrLf +
			"          if (res == true) {" + CrLf +
			"            // Submitting the form to the web application''s completion endpoint." + CrLf +
			"            // TODO: check/update as needed" + CrLf +
			"            document.getElementById(\"asyncresp\").value = response.savetostring();" + CrLf +
			"            document.getElementById(\"respform\").submit();" + CrLf +
			"          }" + CrLf +
			"        }" + CrLf +
			"      }" + CrLf +
			"      xhr.send(hash);" + CrLf +
			"    }" + CrLf +
			"  </script>" + CrLf +
			"</head>" + CrLf +
			"<body style=\"font-family: sans-serif\" onload=''document.getElementById(\"reqhash\").innerHTML=hash;''>" + CrLf +
			"  <h1>DCAuth demo: the web application</h1>" + CrLf +
			"  <h2>Pre-signing completed. </h2>" + CrLf +
			"  The PDFSigner component has successfully pre-signed the provided PDF document." + CrLf +
			"  The pre-signed document has been assigned with a DocID of %s " + CrLf +
			"  and saved to the following location: %s." + CrLf +
			"  <p>" + CrLf +
			"  The async request shown below has been generated as a result of the pre-signing. " + CrLf +
			"  This has also been included in the body of this page as a JSON object, and" + CrLf +
			"  pre-processed to extract the hash. The hash is: <div id=\"reqhash\"></div>. " + CrLf +
			"  This hash value will be submitted to your local signing endpoint by " + CrLf +
			"  the embedded Javascript when you click the Sign" + CrLf +
			"  button below. The signing procedure is performed behind the scenes and " + CrLf +
			"  is fully transparent for you." + CrLf +
			"  <p>" + CrLf +
			"  <textarea cols=\"50\" rows=\"8\">%s</textarea>." + CrLf +
			"  <p>" + CrLf +
			"  This page expects the signing endpoint to be accessible at %s." + CrLf +
			"  If the endpoint cannot be reached at this address, the signing will fail." + CrLf +
			"  <p>" + CrLf +
			"  If the signing completes successfully and a good async response containing" + CrLf +
			"  the signature has been received from the signing service, this page will submit " + CrLf +
			"  it back to the web app for completion of the signing operation. The page " + CrLf +
			"  will notify you when this happens. " + CrLf +
			"  <p>Now click the Sign button to pass the hash to the signing service: " + CrLf +
			"  <button onclick=\"submitAsyncRequest()\">Sign</button>" + CrLf +
			"  <p><div id=\"asyncresptext\"></div>" + CrLf +
			"  <form action=\"%s\" method=\"post\" id=\"respform\">" + CrLf +
			"    <input type=\"hidden\" value=\"\" id=\"asyncresp\" name=\"asyncresp\" />" + CrLf +
			"  </form>" + CrLf +
			"</body>" + CrLf +
			"</html>";

		log("The Java script pre-signing page handler starting.");

		try
		{
			// Recovering the PDF document from the POST, which is passed as a part of a multipart HTTP request
			byte[] buf;
			try
			{
				webserver.config("RequestFilter=parts[0]");
				buf = webserver.getRequestBytes(connectionID);
			}
			finally
			{
				webserver.config("RequestFilter=");
			}

			log("Received a document (" + buf.length + " bytes). Initiating the pre-signing.");

			// Generating a unique document ID.
			String docID = "";
			do
				docID = Long.toHexString(System.currentTimeMillis());
			while (new File(preSignedDocFolder + "\\" + docID).exists());

			// Passing the document for pre-signing. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			String asyncReq = preSignDocument(buf, docID, connectionID);
			if (asyncReq == "")
				return;

			log("Pre-signing completed; DocID is " + docID + ", async request is " + asyncReq.length() + " bytes long.");
			log("Producing the page with embedded async request.");

			String signingService = "";
			if (usePKCS7Method)
				signingService = signingServiceURL + PKCS7Format;
			else
				signingService = signingServiceURL + PKCS1Format;

			// Forming the page by inserting a variety of variable pieces to the template.
			// Note: since AsyncReq is an XML document, we need to URLEncode it to ensure
			// the page's syntactical safety. The signing service URLDecodes it.
			String body = String.format(PageTpl,
					preSigningEndpoint,                // to format a Javascript URI
					asyncReq,                          // async request body in JSON, embedded in the Javascript.
					completionEndpoint,                // completion URI embedded in the Javascript
					signingService,                    // the signing service URI embedded in the Javascript
					docID,                             // DocumentID: we are only including it for informational purposes here
					preSignedDocFolder + "\\" + docID, // A path to the presigned document (again, for informational purposes)
					htmlEncode(asyncReq, true),         // a copy of async request to be shown on the page (HTMLEncoded for safety)
					signingService,                    // the signing service URI again (for informational purposes, to be shown on the page)
					completionEndpoint                 // the completion URI in the form's "action" parameter.
			);

			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseString(connectionID, body, "text/html");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
		}
	}

	// Outputs the DCAuth Javascript file.
	public static void flushDCAuthJS(long connectionID)
	{
		// WARNING! THIS IS A DUMMY PLACEHOLDER. THIS NEEDS TO BE UPDATED TO LOAD
		// THE JAVASCRIPT FILE CAREFULLY.

		log("DCAuth Javascript requested.");
		try
		{
			byte[] buf = Files.readAllBytes(Paths.get(dcAuthJSPath));

			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseBytes(connectionID, buf, "text/javascript");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
		}
	}

	// Completes the signature and saves a fully signed document. This is produced
	// in response to a Javascript POST containing the async response from the pre-signing page.
	public static void completionPage(long connectionID)
	{
		String pageTpl =
			"<html>" + CrLf +
			"<head><title>DCAuth demo: signing completed</title></head>" + CrLf +
			"<body style=\"font-family: sans-serif\">" + CrLf +
			"  <h1>DCAuth demo: the web application</h1>" + CrLf +
			"  <h2>Signing completed successfully</h2>" + CrLf +
			"  The remote signing procedure for your document has completed successfully. " + CrLf +
			"  Click <a href=\"%s\" target=\"_blank\">here</a> to download the signed document. Click <a href=\"/\">here</a> to start again." + CrLf +
			"</body>" + CrLf +
			"</html>";

		log("The completion page handler starting.");

		String resp = "";
		try
		{
			// the async response is provided in a simple URLEncoded form
			try
			{
				webserver.config("RequestFilter=parts[0]");
				resp = webserver.getRequestString(connectionID);
			}
			finally
			{
				webserver.config("RequestFilter=");
			}

			// We are using a very simple param provision procedure here not to overcomplicate things.
			// In a real-world app you can use as complex form as you like, including
			// other parameters alongside the async response.
			if (resp.startsWith("asyncresp="))
				resp = resp.substring(10);
    		else
				throw new Exception("Unexpected request: asyncresp parameter expected");

			log("Received async response(" + resp.length() + " bytes). Completing the signing.");

			// Completing the signing. This call return us a DocID so we could form a download link.
			String docID = completeSigning(resp, connectionID);
			if (docID == "")
				return;

			log("Signing completed for DocID: " + docID);
			log("Producing the page with a link to the signed document.");

			String body = String.format(pageTpl, getFileEndpoint + "?id=" + docID);

			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseString(connectionID, body, "text/html");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
		}
	}

	// Outputs welcome/information page. It is only used for debug/testing purposes,
	// a live signing service does not need it.
	public static void welcomePage(long connectionID)
	{
		String pageTpl =
			"<html>" + CrLf +
			"<head><title>DCAuth demo: welcome</title></head>" + CrLf +
			"<body style=\"font-family: sans-serif\">" + CrLf +
			"  <h1>DCAuth demo: the web application</h1>" + CrLf +
			"  <h2>Welcome to DCAuth demo</h2>" + CrLf +
			"  When you click the Start button, a chosen PDF document will be uploaded" + CrLf +
			"  (POST'ed) to the pre-signing page. During the generation of the pre-signing" + CrLf +
			"  page the following will happen: " + CrLf +
			"  <ul>" + CrLf +
			"    <li>A document hash will be calculated and incorporated into an &quot;async request&quot;," + CrLf +
			"    <li>The async request will be incorporated into the body of the produced page, " + CrLf +
			"    <li>A pre-signed copy of the document will be saved to the special directory on the web server." + CrLf +
			"  </ul>" + CrLf +
			"  When your browser receives the output of the pre-signing page," + CrLf +
			"  it will run the Javascript embedded into the page to pass the async request" + CrLf +
			"  to the local signing service and ask it to sign the hash." + CrLf +
			"  This web application is configured to assume that the signing" + CrLf +
			"  service is accessible from the browser at the following location: %s. " + CrLf +
			"  Please make sure the signing service is active before proceeding to the " + CrLf +
			"  pre-signing stage." + CrLf +
			"  <p>" + CrLf +
			"  If you are using the second part of the DCAuth demo as your signing service," + CrLf +
			"  you can check its availability by following <a href=\"%s\" target=\"_blank\">this link</a>" + CrLf +
			"  (the service should respond with a welcome/status page)" + CrLf +
			"  <form action=\"%s\" method=\"post\" enctype=\"multipart/form-data\">" + CrLf +
			"    Please choose a PDF file to sign: <input type=\"file\" name=\"pdffile\" /><br />" + CrLf +
			"    <input type=\"submit\" value=\"Start signing\" />" + CrLf +
			"  </form>" + CrLf +
			"</body>" + CrLf +
			"</html>";

		log("Producing the welcome page");

		// Forming the page on the basis of a template
		String body = String.format(pageTpl,
			signingServiceURL, // the value included on the page
			signingServiceURL, // the link target
			preSigningEndpoint // the form action
  		);

		try
		{
			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseString(connectionID, body, "text/html");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
		}
	}

	// Just flushes the signed PDF file.
	public static void flushFile(long connectionID)
	{
		log("Flushing a file");

		String id = "";
		try
		{
			// The id of the file is provided as the 'id' parameter
			try
			{
				webserver.config("RequestFilter=params[0]");
				id = webserver.getRequestString(connectionID);
			}
			finally
			{
				webserver.config("RequestFilter=");
			}

			if (id.startsWith("id="))
				id = id.substring(3);
    		else
				throw  new Exception("Unexpected request: id parameter expected.");

			log("File requested: " + id);

			if (!(new File(signedDocFolder + "\\" + id + ".pdf").exists()))
				throw new Exception("Signed file with this ID has not been found");

			// Reading the file contents
			byte[] data = Files.readAllBytes(Paths.get(signedDocFolder + "\\" + id + ".pdf"));

			// Flushing the PDF file out
			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseBytes(connectionID, data, "application/pdf");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
		}
	}

	// Outputs the error page
	public static void errorPage(long connectionID, String errorMsg)
	{
		String pageTpl =
			"<html>" + CrLf +
			"<head><title>DCAuth demo: error</title></head>" + CrLf +
			"<body style=\"font-family: sans-serif\">" + CrLf +
			"  <h1>DCAuth demo: the web application</h1>" + CrLf +
			"  <h2>Error</h2>" + CrLf +
			"  The following error happened when trying to complete the operation: " + CrLf +
			"  <p><p>%s." + CrLf +
			"  <p>Click <a href=\"/\">here</a> to return to the main page." + CrLf +
			"</body>" + CrLf +
			"</html>";

		log("Producing the error page with message \"" + errorMsg + "\"");

		try
		{
			String body = String.format(pageTpl, errorMsg);

			webserver.setResponseStatus(connectionID, 500);
			webserver.setResponseString(connectionID, body, "text/html");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
		}
	}

	// Implements the pre-signing logic. Takes an unsigned PDF on input,
	// saves the pre-signed copy in the local directory, returns the async request
	// and a randomly generated DocID for the document.
	public static String preSignDocument(byte[] docData, String docID, long connectionID)
	{
		// This method pre-signs an unsigned document and produces the async request.

		// Specifically, the SignAsyncBegin call performs the following steps:
		//	- calculates the document hash and prepares any necessary signature elements,
		//	- packs the hash into a DCAuth async request and signs it with KeyID and KeySecret,
		//	- generates a pre-signed version of the document, which is basically a
		//	  document with a placeholder for the future signature.

		// Running the pre-signing routine.
		Pdfsigner signer = new Pdfsigner();
		try
		{
			// Assigning the unsigned document
			signer.setInputBytes(docData);

			// Cancelling any chain validation since we are using a test certificate
			signer.setIgnoreChainValidationErrors(true);
			signer.config("AutoCollectRevocationInfo=false");

			// For the same reason setting the signature level to BES
			// (it is unlikely that the test certificate has any proper chain)
			signer.getSignature().setLevel(secureblackbox.PDFSignature.pslBES);

			// KeyID and KeySecret are arbitrary strings (the longer, the better),
			// but they must match those used by the signing service on the other side.
			signer.getExternalCrypto().setKeyID(keyID);
			signer.getExternalCrypto().setKeySecret(keySecret);

			// Specifying the signing method
			if (usePKCS7Method)
				signer.getExternalCrypto().setMethod(secureblackbox.ExternalCrypto.asmdPKCS7);
    		else
				signer.getExternalCrypto().setMethod(secureblackbox.ExternalCrypto.asmdPKCS1);

			if (useJSMode)
				signer.getExternalCrypto().setMode(secureblackbox.ExternalCrypto.ecmDCAuthJSON);
			else
				signer.getExternalCrypto().setMode(secureblackbox.ExternalCrypto.ecmDCAuth);

			// the signing certificate is REQUIRED for PKCS#1, but optional for PKCS#7
			signer.setSigningCertificate(certmanager.getCertificate());

			// If the certificate was not provided (meaning PKCS#7 mode is used),
			// we need to tune-up the signature widget manually. If the certificate
			// is available, the signer information for the widget will be automatically
			// generated from the values contained in it.
			if (signer.getSigningCertificate() == null)
			{
				signer.getSignature().setAlgorithmCaption("RSA");
				signer.getSignature().setAlgorithmInfo("Strong Asymmetric Algorithm");
				signer.getSignature().setHeader("Good signature");
				signer.getSignature().setSignerCaption("Signed by: ");
				signer.getSignature().setSignerInfo("Trusted Signer");
			}

			// Including DocID in the request as 'user data': DCAuth will mirror it
			// in its async response, and we will use it on the completion stage to identify
			// the document the response corresponds to.
			signer.getExternalCrypto().setData(docID);

			// Setting the TSA service URL. Even though the TSA is only used on the
			// completion stage, we must assign it on the pre-signing stage too,
			// as PDFSigner uses this knowledge when calculating the size of the
			// signature window.
			signer.setTimestampServer(TSAURL);

			// Pre-signing the document.
			String res = signer.signAsyncBegin();

			// Saving the pre-signed document to a local directory.
			try (FileOutputStream fos = new FileOutputStream(preSignedDocFolder + "\\" + docID)) {
				fos.write(signer.getOutputBytes());
			}

			return res;
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
			return "";
		}
	}

	// Implements the completion logic. Takes an async response on input,
	// collects the pre-signed copy from a local directory, embeds the signature,
	// and saves the resulting document in another local directory.
	public static String completeSigning(String asyncResponse, long connectionID)
	{
		// This method completes the signing by inserting the signature into the earlier pre-signed document.

		// Specifically, the SignAsyncEnd call performs the following steps:
		//	- extracts the signature from the async response,
		//	- validates its integrity,
		//	- embeds the signature, along with any other necessary elements, to the signature,
		//	- if the TSA URL was provided, timestamps the created signature.

		Pdfsigner signer = new Pdfsigner();
		try
		{
			if (useJSMode)
				signer.getExternalCrypto().setMode(secureblackbox.ExternalCrypto.ecmDCAuthJSON);
			else
				signer.getExternalCrypto().setMode(secureblackbox.ExternalCrypto.ecmDCAuth);

			// Extracting the DocID from the 'user data' mirrored by the DCAuth
			// service in its async response.
			String docID = signer.extractAsyncData(asyncResponse);

			// Checking if a pre-signed file with such DocID exists.
			if (!(new File(preSignedDocFolder + "\\" + docID).exists()))
				throw new Exception("Pre-signed file not found");

			// Loading the pre-signed document.
			byte[] preSignedDoc = Files.readAllBytes(Paths.get(preSignedDocFolder + "\\" + docID));

			// Assigning the pre-signed document
			signer.setInputBytes(preSignedDoc);

			// Cancelling any chain validation.
			signer.setIgnoreChainValidationErrors(true);

			// Assigning credentials.
			signer.getExternalCrypto().setKeyID(keyID);
			signer.getExternalCrypto().setKeySecret(keySecret);

			// Assigning the timestamping service URL. This time the signer object
			// makes real use of it.
			signer.setTimestampServer(TSAURL);

			// Completing the signing.
			signer.signAsyncEnd(asyncResponse);

			// Saving the now fully signed document to a local directory.
			try (FileOutputStream fos = new FileOutputStream(signedDocFolder + "\\" + docID + ".pdf")) {
				fos.write(signer.getOutputBytes());
			}

			return docID;
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
			return "";
		}
	}
}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof SecureBlackboxException) {
      System.out.print(" (" + ((SecureBlackboxException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



