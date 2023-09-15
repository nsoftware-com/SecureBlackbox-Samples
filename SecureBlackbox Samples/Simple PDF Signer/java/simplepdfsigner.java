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

public class simplepdfsigner extends ConsoleDemo {

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
		if (args.length < 4) {
			System.out.println("usage: simplepdfsigner [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password]");
			System.out.println("                       [-pkcs11 pkcs11_storege_file] [-pin -pkcs11_storage_pin] [-win32 win32_store_name]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to sign (Required).");
			System.out.println("  -output       Where the signed file will be saved (Required).");
			System.out.println("  -cert         The certificate used to sign file.");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -pkcs11       The pkcs11 storage used to sign file.");
			System.out.println("  -pin          The PIN for pkcs11 storage");
			System.out.println("  -win32        The win32 store name");
			System.out.println("\r\nExample: simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword");
			System.out.println("             simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -pkcs11 C:\\pkcs11\\pkcs11.dll -pin mypassword");
			System.out.println("             simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -win32 My");
		} else {
			try {
				Pdfsigner signer = new Pdfsigner();
				Certificatestorage certstorage = new Certificatestorage();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of PDFSigner component for signing PDF documents. *");
				System.out.println("***************************************************************************************************\n");

				String certFile = "";
				String certPass = "";
				String pkcs11File = "";
				String pin = "";
				String win32Store = "My";

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							signer.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							signer.setOutputFile(args[i + 1]);
						if (args[i].equals("-cert"))
							certFile = args[i + 1];
						if (args[i].equals("-certpass"))
							certPass = args[i + 1];
						if (args[i].equals("-pkcs11"))
							pkcs11File = args[i + 1];
						if (args[i].equals("-pin"))
							pin = args[i + 1];
						if (args[i].equals("-win32"))
							win32Store = args[i + 1];
					}
				}

				if (certFile.isEmpty() && pkcs11File.isEmpty() && win32Store.isEmpty()) {
					System.out.println("-cert or -pkcs11 or -win32 is required.");
					return;
				}

				if (!certFile.isEmpty() && !pkcs11File.isEmpty() && !win32Store.isEmpty()) {
					System.out.println("Use only one -cert or -pkcs11 or -win32 parameter.");
					return;
				}

				if (!certFile.isEmpty())
				{
					signer.setSigningCertificate(loadCertificate(certFile, certPass));
				}
				else
				{
					try
					{
						if (!pkcs11File.isEmpty())
						{
							certstorage.open("pkcs11://user:" + pin + "@/" + pkcs11File);
						}
						else
						{
							certstorage.open("system://?store=" + win32Store);
						}

						signer.setSigningCertificate(certstorage.getCertificates().item(0));
					}
					catch (Exception ex)
					{
						System.out.println("Cannot load certificate!");
						return;
					}
				}

				signer.getNewSignature().setLevel(PDFSignature.pslBES);
				signer.getWidget().setInvisible(false);
				signer.setIgnoreChainValidationErrors(true);

				signer.sign();

				System.out.println("PDF file successfully signed");

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



