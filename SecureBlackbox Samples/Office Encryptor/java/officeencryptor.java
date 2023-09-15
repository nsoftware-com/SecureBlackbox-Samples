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

public class officeencryptor extends ConsoleDemo {

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
			System.out.println("usage: officeencryptor [-input input_file] [-output output_file] [-pass encryption_password] [-enctype encryption_type] [-encalg encryption_algorithm]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to encrypt (Required).");
			System.out.println("  -output       Where the encrypted file will be saved (Required).");
			System.out.println("  -pass         Password for file encryption (Required).");
			System.out.println("  -enctype      The type of encryption to use. Enter the corresponding number. Valid values:");
			System.out.println("                  0 - oetDefault");
			System.out.println("                  1 - oetBinaryRC4");
			System.out.println("                  2 - oetBinaryRC4CryptoAPI");
			System.out.println("                  3 - oetOpenXMLStandard");
			System.out.println("                  4 - oetOpenXMLAgile");
			System.out.println("                  5 - oetOpenDocument");
			System.out.println("  -encalg       The encryption algorithm to use. Valid values: RC2, RC4, DES, 3DES, AES128, AES192, AES256, Blowfish");
			System.out.println("\r\nExample: officeencryptor -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword");
		} else {
			try {
				Officeencryptor encryptor = new Officeencryptor();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of OfficeEncryptor component for encrypt office documents. *");
				System.out.println("***************************************************************************************************\n");

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							encryptor.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							encryptor.setOutputFile(args[i + 1]);
						if (args[i].equals("-pass"))
							encryptor.setPassword(args[i + 1]);
						if (args[i].equals("-enctype"))
							encryptor.setEncryptionType(Integer.parseInt(args[i + 1]));
						if (args[i].equals("-encalg"))
							encryptor.setEncryptionAlgorithm(args[i + 1]);
					}
				}

				encryptor.encrypt();

				System.out.println("Office document successfully encrypted");
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



