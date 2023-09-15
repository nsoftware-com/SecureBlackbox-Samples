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

public class soapsigner extends ConsoleDemo {

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

	public static void main(String[] args) {
		if (args.length < 6) {
			System.out.println("usage: soapsigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] [-signbody] [-hashalg hash_algorithm] [-sigtype signature_type]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to sign (Required).");
			System.out.println("  -output       Where the signed file will be saved (Required).");
			System.out.println("  -cert         The certificate used to sign file (Required).");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -signbody     Whether to sign body.");
			System.out.println("  -hashalg      The hash algorithm to use. Valid values: SHA1, MD5, SHA256, SHA384, SHA512, RIPEMD160");
			System.out.println("  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:");
			System.out.println("                  1 - sstWSSSignature");
			System.out.println("                  2 - sstSOAPSignature");
			System.out.println("\r\nExample: soapsigner -input C:\\soap\\myfile.xml -output C:\\soap\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword");
		} else {
			try {
				Soapsigner signer = new Soapsigner();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of Soapsigner component to create SOAP or WSS signatures. *");
				System.out.println("***************************************************************************************************\n");

				String certFile = "";
				String certPass = "";

				signer.getNewSignature().setSignatureType(SOAPSignature.sstWSSSignature);

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							signer.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							signer.setOutputFile(args[i + 1]);
						if (args[i].equals("-signbody"))
							signer.addBodyReference("", true);
						if (args[i].equals("-hashalg"))
							signer.getNewSignature().setHashAlgorithm(args[i + 1]);
						if (args[i].equals("-sigtype"))
							signer.getNewSignature().setSignatureType(Integer.parseInt(args[i + 1]));
						if (args[i].equals("-cert"))
							certFile = args[i + 1];
						if (args[i].equals("-certpass"))
							certPass = args[i + 1];
					}
				}

				if (certFile.isEmpty()) {
					System.out.println("-cert is required.");
					return;
				}

				signer.setSigningCertificate(loadCertificate(certFile, certPass));

				signer.sign();

				System.out.println("SOAP message successfully signed");

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



