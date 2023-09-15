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

public class officedecryptor extends ConsoleDemo {

	public static void main(String[] args) {
		if (args.length < 6) {
			System.out.println("usage: officedecryptor [-input input_file] [-output output_file] [-pass decryption_password]");
			System.out.println("Options: ");
			System.out.println("  -input        An input file to decrypt (Required).");
			System.out.println("  -output       Where the decrypted XML file will be saved (Required).");
			System.out.println("  -pass         The password for the decrypting (Required).");
		} else {
			try {
				Officedecryptor decryptor = new Officedecryptor();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of OfficeDecryptor component for decrypting office documents. *");
				System.out.println("***************************************************************************************************\n");

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							decryptor.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							decryptor.setOutputFile(args[i + 1]);
						if (args[i].equals("-pass"))
							decryptor.setPassword(args[i + 1]);
					}
				}

				decryptor.decrypt();

				System.out.println("Office document successfully decrypted");

			} catch (Exception ex) {
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



