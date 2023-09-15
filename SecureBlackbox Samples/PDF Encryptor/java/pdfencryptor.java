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

public class pdfencryptor extends ConsoleDemo {

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
			System.out.println("usage: pdfencryptor [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] [-pass key_password]");
			System.out.println("                    [-encalg encryption_algorithm] [-nometadata]");
			System.out.println("Options: ");
			System.out.println("  -input        An input PDF file to encrypt (Required).");
			System.out.println("  -output       Where the encrypted PDF file will be saved (Required).");
			System.out.println("  -cert         The certificate used to encrypt file.");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -pass         The password for the encrypting.");
			System.out.println("  -encalg       The encryption algorithm to use. Valid values: RC4, AES128, AES192, AES256, Camellia128, Camellia192, Camellia256, SEED.");
			System.out.println("  -nometadata   Specifies metadata should not be encrypted.");
			System.out.println("\r\nExample: pdfencryptor -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword");
		} else {
			try {
				Pdfencryptor encryptor = new Pdfencryptor();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of PDFEncryptor component for encrypting PDF documents. *");
				System.out.println("***************************************************************************************************\n");

				String certFile = "";
				String certPass = "";
				String userPass = "";

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							encryptor.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							encryptor.setOutputFile(args[i + 1]);
						if (args[i].equals("-encalg"))
							encryptor.setEncryptionAlgorithm(args[i + 1]);
						if (args[i].equals("-nometadata"))
							encryptor.setEncryptMetadata(false);
						if (args[i].equals("-cert"))
							certFile = args[i + 1];
						if (args[i].equals("-certpass"))
							certPass = args[i + 1];
						if (args[i].equals("-pass"))
							userPass = args[i + 1];
					}
				}

				if (userPass.isEmpty() && certFile.isEmpty())
				{
					System.out.println("-cert or -pass is required");
					return;
				}

				if (userPass.isEmpty())
				{
					encryptor.setEncryptionType(Pdfencryptor.petCertificate);
					encryptor.setEncryptionCertificate(loadCertificate(certFile, certPass));
				}
				else
				{
					encryptor.setEncryptionType(Pdfencryptor.petPassword);
					encryptor.setUserPassword(userPass);
				}

				encryptor.encrypt();

				System.out.println("PDF file successfully encrypted");
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



