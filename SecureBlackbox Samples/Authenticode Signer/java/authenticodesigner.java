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

public class authenticodesigner extends ConsoleDemo {

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
		if (args.length < 8) {
			System.out.println("usage: authenticodesigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]");
			System.out.println("                          [-sigurl sigurl] [-hashalg hashalg] [-individual] [-remove] [-tsserver timestamp_server]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to sign (Required).");
			System.out.println("  -output       Where the signed file will be saved (Required).");
			System.out.println("  -cert         The certificate used to sign files (Required).");
			System.out.println("  -certpass     The password for the signing certificate (Required).");
			System.out.println("  -sigurl       The signature URL.");
			System.out.println("  -hashalg      The Hash algorithm.");
			System.out.println("  -individual   Whether to use individual signatures.");
			System.out.println("  -tsserver     A timestamp server to use during signing.");
			System.out.println("  -remove       Whether to remove existing signature.");
			System.out.println("\r\nExample: authenticodesigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword");
			System.out.println("             authenticodesigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword -hashalg SHA256 -individual -remove -tsserver http://timestamp.wosign.com");
		} else {
			try {
				Authenticodesigner signer = new Authenticodesigner();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of Authenticodesigner component for signing EXE and DLL files. *");
				System.out.println("***************************************************************************************************\n");

				String certFile = "";
				String certPass = "";
				boolean individual = false;

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							signer.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							signer.setOutputFile(args[i + 1]);
						if (args[i].equals("-sigurl"))
							signer.setSignatureURL(args[i + 1]);
						if (args[i].equals("-hashalg"))
							signer.setHashAlgorithm(args[i + 1]);
						if (args[i].equals("-tsserver"))
							signer.setTimestampServer(args[i + 1]);
						if (args[i].equals("-individual"))
							individual = true;
						if (args[i].equals("-remove"))
							signer.setRemoveExistingSignatures(true);
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

				if (individual)
					signer.setStatementType(Authenticodesigner.acsIndividual);
				else
					signer.setStatementType(Authenticodesigner.acsCommercial);

				signer.sign();

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



