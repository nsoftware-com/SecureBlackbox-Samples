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
import secureblackbox.*;

public class soapverifier extends ConsoleDemo {

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


	static boolean showrefs = false;

	public static void main(String[] args) {
		if (args.length < 3) {
			System.out.println("usage: soapverifier [-input input_file] [-cert certificate_file] [-certpass certificate_password] [-showrefs]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to verify (Required).");
			System.out.println("  -cert         The certificate used to verify file (Required).");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -showrefs     Whether to display detailed results of reference verification.");
			System.out.println("\r\nExample: soapverifier -input C:\\soap\\myfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword");
		} else {
			try {
				Soapverifier verifier = new Soapverifier();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of Soapsigner component to verify the signature. *");
				System.out.println("***************************************************************************************************\n");

				String certFile = "";
				String certPass = "";

				verifier.addSoapverifierEventListener(new SoapverifierEventListener() {
					@Override
					public void chainElementDownload(SoapverifierChainElementDownloadEvent soapverifierChainElementDownloadEvent) {

					}

					@Override
					public void chainElementNeeded(SoapverifierChainElementNeededEvent soapverifierChainElementNeededEvent) {

					}

					@Override
					public void chainElementStore(SoapverifierChainElementStoreEvent soapverifierChainElementStoreEvent) {

					}

					@Override
					public void chainValidated(SoapverifierChainValidatedEvent soapverifierChainValidatedEvent) {

					}

					@Override
					public void chainValidationProgress(SoapverifierChainValidationProgressEvent soapverifierChainValidationProgressEvent) {

					}

					@Override
					public void error(SoapverifierErrorEvent soapverifierErrorEvent) {

					}

					@Override
					public void messageLoaded(SoapverifierMessageLoadedEvent soapverifierMessageLoadedEvent) {

					}

					@Override
					public void notification(SoapverifierNotificationEvent soapverifierNotificationEvent) {

					}

					@Override
					public void referenceValidated(SoapverifierReferenceValidatedEvent e) {
						if (showrefs)
						{
							String valid = "false";
							if (e.digestValid)
								valid = "true";
							System.out.println(e.ID + "	" + e.URI + "	" + e.refType + "	" +  valid);
						}
					}

					@Override
					public void resolveReference(SoapverifierResolveReferenceEvent soapverifierResolveReferenceEvent) {

					}

					@Override
					public void signatureFound(SoapverifierSignatureFoundEvent soapverifierSignatureFoundEvent) {

					}

					@Override
					public void signatureValidated(SoapverifierSignatureValidatedEvent soapverifierSignatureValidatedEvent) {

					}

					@Override
					public void timestampFound(SoapverifierTimestampFoundEvent soapverifierTimestampFoundEvent) {

					}

					@Override
					public void timestampValidated(SoapverifierTimestampValidatedEvent soapverifierTimestampValidatedEvent) {

					}

					@Override
					public void TLSCertNeeded(SoapverifierTLSCertNeededEvent soapverifierTLSCertNeededEvent) {

					}

					@Override
					public void TLSCertValidate(SoapverifierTLSCertValidateEvent soapverifierTLSCertValidateEvent) {

					}

					@Override
					public void TLSEstablished(SoapverifierTLSEstablishedEvent soapverifierTLSEstablishedEvent) {

					}

					@Override
					public void TLSHandshake(SoapverifierTLSHandshakeEvent soapverifierTLSHandshakeEvent) {

					}

					@Override
					public void TLSShutdown(SoapverifierTLSShutdownEvent soapverifierTLSShutdownEvent) {

					}

					@Override
					public void supercoreIntercept(SoapverifierSupercoreInterceptEvent soapverifierSupercoreInterceptEvent) {

					}
				});

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							verifier.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-showrefs"))
							showrefs = true;
						if (args[i].equals("-cert"))
							certFile = args[i + 1];
						if (args[i].equals("-certpass"))
							certPass = args[i + 1];
					}
				}

				if (!certFile.isEmpty()) {
					verifier.getKnownCertificates().add(loadCertificate(certFile, certPass));
				}

				if (showrefs)
				{
					System.out.println("ID	URI	RefType	DigestValid");
				}

				verifier.verify();

				System.out.println("There are " + verifier.getSignatures().size() + " signatures in this file.\n");
				for (Integer i = 0; i < verifier.getSignatures().size(); i++)
				{
					SOAPSignature sig = verifier.getSignatures().item(i);
					System.out.println("Signature " + (i + 1) + "\n");
					System.out.println("  Claimed signing time: " + sig.getClaimedSigningTime() + "\n");
					String s = "";
					switch (sig.getSignatureValidationResult()) {
						case SOAPSignature.xsvValid: s = "Valid"; break;
						case SOAPSignature.xsvCorrupted: s = "Corrupted"; break;
						case SOAPSignature.xsvSignerNotFound: s = "SignerNotFound"; break;
						case SOAPSignature.xsvFailure:  s = "Failure"; break;
						case SOAPSignature.xsvReferenceCorrupted: s = "ReferenceCorrupted"; break;
						default: s = "Unknown"; break;
					}

					System.out.println("Signature validation result: " + s + "\n");
					s = "";
					switch (sig.getChainValidationResult())
					{
						case SOAPSignature.cvtValid: s = "Valid"; break;
						case SOAPSignature.cvtValidButUntrusted: s = "ValidButUntrusted"; break;
						case SOAPSignature.cvtInvalid: s = "Invalid"; break;
						case SOAPSignature.cvtCantBeEstablished: s = "CantBeEstablished"; break;
						default: s = "Unknown"; break;
					}

					System.out.println("Chain Validation Result: " + s + "\n");
				}
			}
			catch (Exception ex)
			{
				displayError(ex);
			}
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



