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

public class authenticodeverifier extends ConsoleDemo {

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

	public static String translateSigValidity(int type)
	{
		switch (type) {
			case Authenticodeverifier.svtValid:
				return "Valid";
			case Authenticodeverifier.svtCorrupted:
				return "Corrupted";
			case Authenticodeverifier.svtSignerNotFound:
				return "Signer not found";
			case Authenticodeverifier.svtFailure:
				return "Failure";
			default:
				return "Unknown";
		}
	}

	public static void main(String[] args) {
		if (args.length < 6) {
			System.out.println("usage: authenticodeverifier [-input input_file] [-cert certificate_file] [-certpass certificate_password]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to verify (Required).");
			System.out.println("  -cert         The certificate used to sign files (Required).");
			System.out.println("  -certpass     The password for the signing certificate (Required).");
			System.out.println("\r\nExample: authenticodeverifier -input C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword");
		} else {
			try {
				Authenticodeverifier verifier = new Authenticodeverifier();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of Authenticodeverifier component for verify the signature. *");
				System.out.println("***************************************************************************************************\n");

				String certFile = "";
				String certPass = "";

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							verifier.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
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

				verifier.getKnownCertificates().add(loadCertificate(certFile, certPass));

				verifier.verify();

				if (!verifier.isSigned())
				{
					System.out.println("The file is not singed.");
				}
				else {
					System.out.println("There are " + verifier.getSignatures().size() + " signatures in this file.");
					for (int x = 0; x < verifier.getSignatures().size(); x++) {
						System.out.println("Signature #" + (x+1));
						System.out.println("  Hash algorithm: " + verifier.getSignatures().item(x).getHashAlgorithm());
						System.out.println("  Description: " + verifier.getSignatures().item(x).getDescription());
						System.out.println("  URL: " + verifier.getSignatures().item(x).getURL());
						System.out.println("  Validity: " + translateSigValidity(verifier.getSignatures().item(x).getSignatureValidationResult()));
						System.out.println("");
					}
				}
				System.out.println("The file has been successfully signed");

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



