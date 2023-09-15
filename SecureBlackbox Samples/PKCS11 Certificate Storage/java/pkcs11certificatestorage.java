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

public class pkcs11certificatestorage extends ConsoleDemo {

	public static void main(String[] args) {
		if (args.length < 4) {
			System.out.println("usage: pkcs11certificatestorage [-storage pkcs11_storege_file] [-pin -pkcs11_storage_pin]");
			System.out.println("Options: ");
			System.out.println("  -storage      The pkcs11 storage file (Required).");
			System.out.println("  -pin          The PIN for pkcs11 storage.");
			System.out.println("\r\nExample: pkcs11certificatestorage -storage C:\\pkcs11\\pkcs11.dll -pin mypassword");
		} else {
			try {
				Certificatestorage certstorage = new Certificatestorage();
				Certificatestorage certstorage_dop = new Certificatestorage();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of CertificateStorage component for work with PKCS11 storage. *");
				System.out.println("***************************************************************************************************\n");

				String storageFile = "";
				String pin = "";

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-storage"))
							storageFile = args[i + 1];
						if (args[i].equals("-pin"))
							pin = args[i + 1];
					}
				}

				if (storageFile.isEmpty()) {
					System.out.println("-storage is required.");
					return;
				}

				certstorage.open("pkcs11:///" + storageFile + "?slot=-1");

				int slotCount = Integer.parseInt(certstorage.config("PKCS11SlotCount"));

				for (int i = 0; i < slotCount; i++)
				{
					String desc = certstorage.config("PKCS11SlotDescription[" + i + "]");
					String active = certstorage.config("PKCS11SlotTokenPresent[" + i + "]");

					if (desc != "")
					{
						if (active == "True")
						{
							System.out.println(desc + ":");

							certstorage_dop.open("pkcs11://user:" + pin + "@/" + storageFile + "?slot=" + i);

							for (int j = 0; j < certstorage_dop.getCertificates().size(); j++)
							{
								System.out.println("	Subject: " + certstorage_dop.getCertificates().item(j).getSubject());
								System.out.println("	Issuer: " + certstorage_dop.getCertificates().item(j).getIssuer());
								System.out.println("	ValidFrom: " + certstorage_dop.getCertificates().item(j).getValidFrom());
								System.out.println("	ValidTo: " + certstorage_dop.getCertificates().item(j).getValidTo());
								System.out.println("	Key: " + certstorage_dop.getCertificates().item(j).getKeyAlgorithm() + " (" + certstorage_dop.getCertificates().item(j).getKeyBits() + ")");
								System.out.println("");
							}

							System.out.println("");
						}
						else
						{
							System.out.println(desc + ": No token ");
							System.out.println("");
						}
					}
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



