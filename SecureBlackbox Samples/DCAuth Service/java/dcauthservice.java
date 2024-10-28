/*
 * SecureBlackbox 2024 Java Edition - Sample Project
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
import java.awt.*;
import java.net.*;
import java.nio.charset.StandardCharsets;

import secureblackbox.*;

import static neo.CAdESSignatureLevel.*;
import static secureblackbox.CAdESSigner.*;
import static secureblackbox.PublicKeyCrypto.*;

public class dcauthservice extends ConsoleDemo
{
	// The web server that listens to relayed async requests from the browser.
	static HTTPServer webserver = new HTTPServer();

	// The signing endpoint on the web service - e.g. /sign, but can be anything.
	static String signendpoint = "/sign";

	// The DCAuth authentication credentials: KeyID and KeySecret.
	// These two should match the ones used by the pre-signing party, such as PDF signer.
	static String keyid = "mykeyid";
	static String keysecret = "mykeysecret";

	// The path to the signing certificate. This must have an associated private key,
	// so normally it will be in .pfx format. Sometimes PEM is also used, but
	// make sure it does contain the private key.
	static String certfile = "cert.pfx";

	// The password to decrypt the certificate or key.
	static String certpass = "password";

	// disable use java script mode.
	static boolean disablejs = false;

	public static final String CrLf = "\r\n";
	public static final String PKCS1Format = "?format=pkcs1";
	public static final String PKCS7Format = "?format=pkcs7";

	public static int parseWithDefault(String s, int defaultVal) {
		return s.matches("-?\\d+") ? Integer.parseInt(s) : defaultVal;
	}

	public static void main(String[] args) {
		if (args.length < 1) {
			System.out.println("usage: dcauthservice <listening_port> [-endpoint signing_endpoint] [-keyid key_id] [-keysecret key_secret]");
			System.out.println("   [-cert certificate_file] [-certpass certificate_password] [-webapp] [-disablejs]");
			System.out.println("Options: ");
			System.out.println("  -endpoint     The signing endpoint.");
			System.out.println("The listening port and the endpoint should match those configured in the web app.");
			System.out.println("  -keyid        The key ID.");
			System.out.println("  -keysecret    The key Secret.");
			System.out.println("The Key ID and Key Secret should match those used by the signing component on the pre-signing stage");
			System.out.println("  -cert         The certificate used to sign file.");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -webapp       Launch demo web app at default location");
			System.out.println("  -disablejs    Whether to disable javascript mode");
			System.out.println("\r\nExample: dcauthservice 17080");
			System.out.println("             dcauthservice 17080 -endpoint /sign -keyid mykeyid -keysecret mykeysecret -cert C:\\certs\\mycert.pfx -certpass mypassword -webapp\n");

			System.out.println("Press Enter to exit the demo.");
			input();
		}
		else
		{
			try
			{
				System.out.println("***************************************************************************************************");
				System.out.println("This demo application is a part of SecureBlackbox remote signing (DCAuth) demo. It runs a local ");
				System.out.println("HTTP service that can process async requests from the web application that runs in a local browser.");
				System.out.println("It uses a private key residing on a local system to sign hashes included in the async requests and ");
				System.out.println("produce async responses containing the signatures.");
				System.out.println("");
				System.out.println("The web app is the second part of the demo and should be run separately, on this or different system.");
				System.out.println("");
				System.out.println("To evaluate the demo, please do the following:");
				System.out.println("");
				System.out.println("		1) Run this application, set up the parameters of the service (if needed), and start the service.");
				System.out.println("		2) Run the second part of the demo (the web application) on this or different system.");
				System.out.println("		3) Navigate to the web application in your local browser. In default configuration, and when running both");
				System.out.println("		   parts of the demo on the same system, it can be accessed at http://127.0.0.1:16080.");
				System.out.println("		4) Follow the guidance of the web app");
				System.out.println("***************************************************************************************************\n");

				// HTTP request handlers: we want to intercept GETs and POSTs
				webserver.addHTTPServerEventListener(new HTTPServerEventListener() {
					@Override
					public void accept(HTTPServerAcceptEvent e) {

					}

					@Override
					public void authAttempt(HTTPServerAuthAttemptEvent e) {

					}

					@Override
					public void connect(HTTPServerConnectEvent e) {

					}

					@Override
					public void customRequest(HTTPServerCustomRequestEvent e) {

					}

					@Override
					public void data(HTTPServerDataEvent e) {

					}

					@Override
					public void deleteRequest(HTTPServerDeleteRequestEvent e) {

					}

					@Override
					public void disconnect(HTTPServerDisconnectEvent e) {

					}

					@Override
					public void error(HTTPServerErrorEvent e) {

					}

					@Override
					public void externalSign(HTTPServerExternalSignEvent e) {

					}

					@Override
					public void fileError(HTTPServerFileErrorEvent e) {

					}

					@Override
					public void getRequest(HTTPServerGetRequestEvent e) {
						// This event handler fires when the web service receives a GET request.
						// Normally the DCAuth signing service should only respond to POSTs coming
						// to its signing endpoint. In our demo service we also respond to GETs
						// with a welcome message/health status. This is not required in real-life
						// service.

						log("Received GET from connection " + e.connectionID + " for " + e.URI);

						welcomePage(e.connectionID);

						e.handled = true;
					}

					@Override
					public void headersPrepared(HTTPServerHeadersPreparedEvent e) {

					}

					@Override
					public void headRequest(HTTPServerHeadRequestEvent e) {

					}

					@Override
					public void notification(HTTPServerNotificationEvent e) {

					}

					@Override
					public void optionsRequest(HTTPServerOptionsRequestEvent e) {

					}

					@Override
					public void patchRequest(HTTPServerPatchRequestEvent e) {

					}

					@Override
					public void postRequest(HTTPServerPostRequestEvent e) {
						// This event handler fires when the web service receives a POST request.
						// The web app's Javascript uses POST to relay async requests to the signing
						// service. We filter the incoming request by the sign endpoint; requests
						// to any other locations result in a welcome page.

						log("Received POST from connection " + e.connectionID + " for " + e.URI);

						if (e.URI.equals(signendpoint + PKCS1Format))
						{
							if (!disablejs)
								signPageJSMode(e.connectionID, false);
							else
								errorPage(e.connectionID, "Functionality not supported");
						}
						else
						if (e.URI.equals(signendpoint + PKCS7Format))
						{
							if (!disablejs)
								signPageJSMode(e.connectionID, true);
							else
								errorPage(e.connectionID, "Functionality not supported");
						}
						else
						if (e.URI.equals(signendpoint))
							signPage(e.connectionID);
						else
							welcomePage(e.connectionID);

						e.handled = true;
					}

					@Override
					public void putRequest(HTTPServerPutRequestEvent e) {

					}

					@Override
					public void resourceAccess(HTTPServerResourceAccessEvent e) {

					}

					@Override
					public void TLSCertValidate(HTTPServerTLSCertValidateEvent e) {

					}

					@Override
					public void TLSEstablished(HTTPServerTLSEstablishedEvent e) {

					}

					@Override
					public void TLSHandshake(HTTPServerTLSHandshakeEvent e) {

					}

					@Override
					public void TLSPSK(HTTPServerTLSPSKEvent e) {

					}

					@Override
					public void TLSShutdown(HTTPServerTLSShutdownEvent e) {

					}

					@Override
					public void traceRequest(HTTPServerTraceRequestEvent e) {

					}

					@Override
					public void supercoreIntercept(HTTPServerSupercoreInterceptEvent e) {

					}
				});

				certfile = new File(".").getCanonicalPath() + "\\cert.pfx";

				// Launch demo web app at default location on start
				boolean launchwebapp = false;

				for (int i = 1; i < args.length; i++)
				{
					if (args[i].startsWith("-"))
					{
						// The signing endpoint (relative to the local host, e.g. /sign)
						if (args[i].equals("-endpoint"))
							signendpoint = args[i + 1]; // args[i+1] corresponds to the value of argument [i]

						// Authentication credentials
						if (args[i].equals("-keyid"))
							keyid = args[i + 1];
						if (args[i].equals("-keysecret"))
							keysecret = args[i + 1];

						// Signing certificate parameters
						if (args[i].equals("-cert"))
							certfile = args[i + 1];
						if (args[i].equals("-certpass"))
							certpass = args[i + 1];

						if (args[i].equals("-webapp"))
							launchwebapp = true;

						if (args[i].equals("-disablejs"))
							disablejs = true;
					}
				}

				if (!(new File(certfile).exists()))
				{
					System.out.println("Please make sure you provide a signing certificate that includes its private key. The service will be unable to sign async requests otherwise.");
					return;
				}

				//== Preparing the web server

				// Port to listen on
				webserver.setPort(parseWithDefault(args[0], 17080));

				// No need for keep-alive connections
				webserver.setAllowKeepAlive(false);

				// Starting the server
				webserver.start();

				if (launchwebapp)
				{
					String URL = "http://127.0.0.1:16080/";
					System.out.println("The demo will now try to open a web application at its known default location of " + URL + ".");
					System.out.println("Please make sure the web application (the second part of the demo) is running. ");
					System.out.println("If you changed any settings in the web app, this button may fail to work, and you ");
					System.out.println("might need to open the application manually by typing the correct URL in your browser.");
					Desktop.getDesktop().browse(new URI(URL));
				}

				System.out.println("");
				System.out.println("The web endpoint has started on port " + webserver.getPort() + ". Press enter to stop and exit.");
				System.out.println("");
				System.in.read();

				webserver.stop();
				System.out.println("The web endpoint has stopped");
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

	// Outputs welcome/information page. It is only used for debug/testing purposes,
	// a live signing service does not need it.
	public static void welcomePage(long connectionID)
	{
		String pageTpl =
			"<html>" + CrLf +
			"<head><title>DCAuth demo: welcome</title></head>" + CrLf +
			"<body style=\"font-family: sans-serif\">" + CrLf +
			"  <h1>DCAuth demo: the local signing service</h1>" + CrLf +
			"  <h2>Welcome to DCAuth demo</h2>" + CrLf +
			"  This is an information page of the DCAuth signing service. " + CrLf +
			"  This web service is supposed to run in environment reachable " + CrLf +
			"  by the user''s browser - either on their workstation, or on their local" + CrLf +
			"  network." + CrLf +
			"  <p>" + CrLf +
			"  The service listens to async requests forwarded by the browser using " + CrLf +
			"  the Javascript piece embedded into the web page produced by the signing web app. " + CrLf +
			"  It is supposed to run invisibly and should not be normally used directly by humans. " + CrLf +
			"  This page is only provided for debug purposes and as a service health check. " + CrLf +
			"  <font color=\"green\">The service is ready to accept requests</font> and is configured to use the following parameters: " + CrLf +
			"  <p> " + CrLf +
			"  <ul> " + CrLf +
			"    <li>Signing endpoint: %s" + CrLf +
			"    <li>Certificate: %s" + CrLf +
			"    <li>Key ID: %s" + CrLf +
			"    <li>Key Secret: <i>hidden</i>" + CrLf +
			"  </ul> " + CrLf +
			"  <p> " + CrLf +
			"  To evaluate the demo, please follow the steps below: " + CrLf +
			"  <p>" + CrLf +
			"  <ol>" + CrLf +
			"    <li>Make sure the web application (the second part of the demo) is running. " + CrLf +
			"      You can run it on the same or a different computer. Make sure the web app " + CrLf +
			"      uses the right signing endpoint URI (host + resource), and the same Key ID and Key Secret" + CrLf +
			"      as used by this service." + CrLf +
			"    <li>Use your browser to navigate to the web application. In most demo variants, the web application" + CrLf +
			"      listens on http://127.0.0.1:16080, unless modified by the user." + CrLf +
			"    <li>Follow the instructions of the web application to initiate the signing." + CrLf +
			"    <li>At some point the web app will lead you to the page that includes the async request." + CrLf +
			"      When asked, confirm that you want to submit that request to the DCAuth service (THIS service)." + CrLf +
			"    <li>The DCAuth service (THIS service) will then sign the request and produce the response." + CrLf +
			"      This will happen behind the scenes in your browser, which will use Javascript to conduct the exchange." + CrLf +
			"    <li>If asked, confirm that you are happy to submit that response back to the web app." + CrLf +
			"    <li>The web application will then finalize the signing and produce the signed document." + CrLf +
			"  </ol>" + CrLf +
			"  <p>IMPORTANT NOTE: Please read the comment about CORS requirements in the body of the SignPage() method. " + CrLf +
			"  In particular, this service MUST use HTTPS if the web application uses it. " + CrLf +
			"</body>" + CrLf +
			"</html>";

		try
		{
			log("Producing the welcome page");

			String body = String.format(pageTpl, signendpoint, certfile, keyid);

			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseString(connectionID, body, "text/html", "");
		}
		catch (Exception ex)
		{
			displayError(ex);
		}
	}

	// Outputs the content produced by the signing endpoint. This is not really
	// an HTML page; it returns an XML document containing the async response.
	public static void signPage(long connectionID) {
		// This is not a "page" per se, but rather an XML body containing the DCAuth async response.
		// If the response can't be provided for any reason, a 404 error is returned.
		log("Starting the signing handler.");

		String par = "";
		try
		{
			// Obtaining the async request, which is passed as a part of a multipart HTTP request
			try
			{
				webserver.config("RequestFilter=parts[0]");
				par = webserver.getRequestString(connectionID, "");
			}
			finally
			{
				webserver.config("RequestFilter=");
			}

			log("Received what seems to be an async request (" + par.length() + " bytes). Initiating the DCAuth signing routine.");

			// The async request comes in URLEncoded form: we need to decode it first.
			// (the web app has to use URLEncode to safely embed the request's XML data into its HTML page)
			String asyncReq = URLDecoder.decode(par, StandardCharsets.UTF_8.name());

			// Extracting and signing the request. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			String asyncResp = signAsyncRequest(asyncReq);

			log("Signing completed; the async response is " + asyncResp.length() + " bytes long.");
			log("Producing the response.");

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
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Origin", "*");
			// If required, the origin can be restricted using the following parameters:
			// FWebServer.SetResponseHeader(ConnectionId, 'Access-Control-Allow-Origin', 'https://mysite.com');
			// FWebServer.SetResponseHeader(ConnectionId, 'Vary', 'Origin');

			// These extra CORS headers tell what else we are happy to accept
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Methods", "POST, GET");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Headers", "Authorization, Content-Type");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Credentials", "true");

			// Providing the response status and body
			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseString(connectionID, asyncResp, "text/xml", "");
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
		}
	}

	private static byte[] hexDecode(String hex)
	{
		int len = hex.length();
		byte[] data = new byte[len / 2];
		for (int i = 0; i < len; i += 2) {
			data[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4)
					+ Character.digit(hex.charAt(i+1), 16));
		}
		return data;
	}

	private static String hexEncode(byte[] bytes)
	{
		char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray();

		char[] hexChars = new char[bytes.length * 2];
		for (int j = 0; j < bytes.length; j++) {
			int v = bytes[j] & 0xFF;
			hexChars[j * 2] = HEX_ARRAY[v >>> 4];
			hexChars[j * 2 + 1] = HEX_ARRAY[v & 0x0F];
		}
		return new String(hexChars);
	}

	// Takes a hash on input and produces a java script PKCS1 signature over it.
	private static byte[] signPKCS1Hash(byte[] hash, long connectionID)
	{
		//This method takes a hash and signs it using the private key contained
		//in the certificate.This method works in "java script signing" (JSON)
		//mode, and does not involve DCAuth async requests and responses.
		try
		{
			// Loading the certificate
			CertificateManager certmanager = new CertificateManager();
			certmanager.importFromFile(certfile, certpass);

			// Extracting the signing key using the CryptoKeyManager object
			CryptoKeyManager keymanager = new CryptoKeyManager();
			keymanager.setCertificate(certmanager.getCertificate());
			keymanager.importFromCert();

			// Signing the hash
			PublicKeyCrypto crypto = new PublicKeyCrypto();
			crypto.setKey(keymanager.getKey());
			crypto.setHashAlgorithm("SHA256");
			crypto.setInputIsHash(true);
			crypto.setInputEncoding(cetBinary);
			crypto.setOutputEncoding(cetBinary);
			return crypto.sign(hash, true);
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
			return null;
		}
	}

	// Takes a hash on input and produces a java script PKCS7 signature over it.
	private static byte[] signPKCS7Hash(byte[] hash, long connectionID)
	{
		//This method takes a hash and signs it using the private key contained
		//in the certificate.This method works in "java script signing" (JSON)
		//mode, and does not involve DCAuth async requests and responses.

		try
		{
			// Loading the certificate
			CertificateManager certmanager = new CertificateManager();
			certmanager.importFromFile(certfile, certpass);

			// Signing the hash
			CAdESSigner signer = new CAdESSigner();
			signer.setSigningCertificate(certmanager.getCertificate());
			signer.setDetached(true);
			signer.getNewSignature().setLevel(aslBES);
			signer.getNewSignature().setHashAlgorithm("SHA256");
			signer.config("InputIsHash=true");
			signer.setRevocationCheck(crcNone);
			signer.setIgnoreChainValidationErrors(true);
			signer.setInputBytes(hash);

			signer.sign();

			return signer.getOutputBytes();
		}
		catch (Exception ex)
		{
			log("Exception: " + ex.getMessage());
			errorPage(connectionID, ex.getMessage());
			return null;
		}
	}

	// Outputs the content produced by the java script signing endpoint. Just as SignPage(),
	// this is not an HTML page; it returns a hexadecimal-encoded signature value
	// to be placed in the async response.
	public static void signPageJSMode(long connectionID, boolean isPKCS7)
	{
		// This is not a "page" per se, but rather an XML body containing the DCAuth async response.
		// If the response can't be provided for any reason, a 404 error is returned.
		log("Starting the java script signing handler.");

		String par = "";
		try
		{
			// Obtaining the async request, which is passed as a part of a multipart HTTP request
			try
			{
				webserver.config("RequestFilter=parts[0]");
				par = webserver.getRequestString(connectionID, "");
			}
			finally
			{
				webserver.config("RequestFilter=");
			}

			// The hash comes in hex-encoded form
			byte[] hashBytes = hexDecode(par);

			log("Received a hash to be signed (" + hashBytes.length + " bytes). Initiating the plain (non-DCAuth) signing routine.");

			// Extracting and signing the request. If anything goes wrong, an exception
			// will be thrown, taking us to the 'except' handler below.
			byte[] sigBytes;
			if (isPKCS7)
				sigBytes = signPKCS7Hash(hashBytes, connectionID);
			else
				sigBytes = signPKCS1Hash(hashBytes, connectionID);

			log("Signing completed; the async response is " + sigBytes.length + " bytes long.");
			log("Producing the response.");

			// Converting signature to hex
			String sig = hexEncode(sigBytes);

			// See note about CORS in the SignPage() method implementation above.
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Origin", "*");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Methods", "POST, GET");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Headers", "Authorization, Content-Type");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Credentials", "true");

			// Providing the response status and body
			webserver.setResponseStatus(connectionID, 200);
			webserver.setResponseString(connectionID, sig, "text/xml", "");
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
			"  <h1>DCAuth demo: the local signing service</h1>" + CrLf +
			"  <h2>Error</h2>" + CrLf +
			"  The following error has happened when trying to complete the operation: " + CrLf +
			"  <p>%s." + CrLf +
			"</body>" + CrLf +
			"</html>";

		try
		{
			log("Producing the error page with message \"" + errorMsg + "\"");

			// CORS headers (see comment above)
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Origin", "*");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Methods", "POST, GET");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Headers", "Authorization, Content-Type");
			webserver.setResponseHeader(connectionID, "Access-Control-Allow-Credentials", "true");

			String body = String.format(pageTpl, errorMsg);
			webserver.setResponseStatus(connectionID, 404);
			webserver.setResponseString(connectionID, body, "text/html", "");
		}
		catch (Exception ex)
		{
			displayError(ex);
		}
	}

	// Implements the logic of the DCAuth request processing. Takes a DCAuth request
	// on input, and produces a DCAuth response that matches it.
	public static String signAsyncRequest(String request)
	{
		// This method accepts a DCAuth request and produces the matching response.

		// The DCAuth control takes the following steps when processing the request:
		//	- validates the integrity of the request using its KeyID and KeySecret,
		//	- extracts the document hash from the request,
		//	- signs the hash with the provided certificate,
		//	- incorporates the signed hash, along with any certificates (if requested) into
		//	the response state, and returns it.

		DCAuth auth = new DCAuth();
		try {
			// KeyID and KeySecret are arbitrary strings (the longer, the better),
			// but they must match those used by the signing object (e.g. PDFSigner).
			auth.setKeyId(keyid);
			auth.setKeySecret(keysecret);

			// Setting up the signing certificate and its password.
			CertificateManager certmanager = new CertificateManager();
			certmanager.importFromFile(certfile, certpass);
			auth.setSigningCertificate(certmanager.getCertificate());

			// Providing the async request.
			auth.setInput(request);

			// Telling DCAuth to process it.
			auth.processRequest();

			// Grabbing the response state.
			return auth.getOutput();
		}
		catch (Exception ex)
		{
			displayError(ex);
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



