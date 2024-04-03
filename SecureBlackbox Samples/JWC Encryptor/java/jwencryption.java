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
import static secureblackbox.Symmetriccrypto.*;

public class jwencryption extends ConsoleDemo {

	public static void main(String[] args) {
		if (args.length < 5) {
			System.out.println("usage: jwencryption -e/-d [-input input_data] [-pass encryption_password] [-compact] [-encalg encryption_algorithm]");
			System.out.println("Options: ");
			System.out.println("  -e            Whether to encrypt input data.");
			System.out.println("  -d            Whether to decrypt input data.");
			System.out.println("  -input        An input data to encrypt/decrypt (Required).");
			System.out.println("  -pass         Password for encryption (Required).");
			System.out.println("  -compact      Whether to use compact format");
			System.out.println("  -encalg       The encryption algorithm to use. Valid values: RC2, RC4, DES, 3DES, AES128, AES192, AES256, Blowfish");
			System.out.println("\r\nExample: jwencryption -e -input \"And now that you donâ€™t have to be perfect, you can be good.\" -pass mypassword -encalg AES256");
			System.out.println("             jwencryption -d -input eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -pass mypassword -compact");
		} else {
			try {
				Symmetriccrypto crypto = new Symmetriccrypto();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates how to encrypt text to a JW token with a password. *");
				System.out.println("***************************************************************************************************\n");

				boolean encrypt = false;
				boolean decrypt = false;
				String input = "";
				String pass = "";
				boolean compact = false;

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-e"))
							encrypt = true;
						if (args[i].equals("-d"))
							decrypt = true;
						if (args[i].equals("-input"))
							input = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-pass"))
							pass = args[i + 1];
						if (args[i].equals("-compact"))
							compact = true;
						if (args[i].equals("-encalg"))
							crypto.setEncryptionAlgorithm(args[i + 1]);
					}
				}

				if (!(encrypt || decrypt))
				{
					System.out.println("-e or -d is required.");
					return;
				}

				if (encrypt && decrypt)
				{
					System.out.println("Use only one -e or -d parameter.");
					return;
				}

				byte[] inputB = input.getBytes();

				// genarate key from password
				Cryptokeymanager keymanager = new Cryptokeymanager();
				keymanager.deriveKey(256, pass, "");
				keymanager.getKey().setIV(new byte[16]);
				crypto.setKey(keymanager.getKey());

				if (encrypt)
				{
					if (compact)
					{
						crypto.setOutputEncoding(cetCompact);
					}
					else
					{
						crypto.setOutputEncoding(cetJSON);
					}

					byte[] outputB = crypto.encrypt(inputB);

					System.out.println("Encrypted token: " + new String(outputB));
				}
				else
				{
					if (compact)
					{
						crypto.setInputEncoding(cetCompact);
					}
					else
					{
						crypto.setInputEncoding(cetJSON);
					}

					byte[] outputB = crypto.decrypt(inputB);

					System.out.println("Decrypted string: " + new String(outputB));
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



