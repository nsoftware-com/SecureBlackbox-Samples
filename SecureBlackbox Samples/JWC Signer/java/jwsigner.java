/*
 * SecureBlackbox 2024 Java Edition - Sample Project
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
import static secureblackbox.PublicKeyCrypto.*;

public class jwsigner extends ConsoleDemo {

	public static Certificate loadCertificate(String file, String password) {
		Certificate cert = null;

		if (file.length() > 0) {
			try {
				CertificateManager certmanager = new CertificateManager();

				certmanager.importFromFile(file, password);

				cert = certmanager.getCertificate();
			} catch (Exception e) {
				System.out.println("Cannot load certificate!");
			}
		}

		return cert;
	}

	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
						"  jwsigner -- SecureBlackbox SymmetricCrypto Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  jwsigner <-s/-v> <-input input_data> <-cert certificate_file> [-certpass certificate_password] [-sig signature_data] [-compact]\n\n" +
						"DESCRIPTION\n" +
						"  This sample illustrates how to create a detached signature over a text string.\n" +
						"  Used to sign and verify data.\n\n" +
						"  The options are as follows:\n\n" +
						"  -s            Whether to sign input data. \n\n" +
						"  -v            Whether to verify signature data. \n\n" +
						"  -input        An input data to sign/verify (Required). \n\n" +
						"  -cert         The certificate used to encrypt file (Required). \n\n" +
						"  -certpass     The password for the certificate. \n\n" +
						"  -sig          An signature data to verify (Required to verify). \n\n" +
						"  -compact      Whether to use compact format \n\n" +
						"EXAMPLES\n" +
						"	jwsigner -s -input \"And now that you don't have to be perfect, you can be good.\" -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n" +
						"	jwsigner -v -input \"And now that you don't have to be perfect, you can be good.\" \n" +
						"		-sig eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -cert C:\\certs\\mycert.pfx -certpass mypassword -compact \n"
		);

		if (errMes.length() > 0) {
			System.out.println("Error: " + errMes);
			System.out.println();
		}

		confirmExit();
	}

	private static void confirmExit() {
		System.out.println("Press Enter to exit the demo.");
		input();
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		try {
			PublicKeyCrypto crypto = new PublicKeyCrypto();
			boolean sign = false;
			boolean verify = false;
			String input = "";
			String sig = "";
			String certfile = "";
			String certpass = "";
			boolean compact = false;

			for (int i = 0; i < args.length; i++) {
				if (args[i].startsWith("-")) {
					if (args[i].equals("-s"))
						sign = true;
					if (args[i].equals("-v"))
						verify = true;
					if (args[i].equals("-input"))
						input = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
					if (args[i].equals("-sig"))
						sig = args[i + 1];
					if (args[i].equals("-cert"))
						certfile = args[i + 1];
					if (args[i].equals("-certpass"))
						certpass = args[i + 1];
					if (args[i].equals("-compact"))
						compact = true;
				}
			}

			if (!(sign || verify)) {
				displayHelp("-s or -v is required.");
				return;
			}

			if (sign && verify) {
				displayHelp("Use only one -s or -v parameter.");
				return;
			}

			if (input.isEmpty()) {
				displayHelp("-input is required.");
				return;
			}

			if (verify && sig.isEmpty()) {
				displayHelp("-sig is required.");
				return;
			}

			if (certfile.isEmpty()) {
				displayHelp("-cert is required.");
				return;
			}

			byte[] inputB = input.getBytes();

			// load key from certificate file
			CryptoKeyManager keymanager = new CryptoKeyManager();
			keymanager.setCertificate(loadCertificate(certfile, certpass));
			keymanager.importFromCert();
			crypto.setKey(keymanager.getKey());

			if (sign) {
				if (compact) {
					crypto.setOutputEncoding(cetCompact);
				} else {
					crypto.setOutputEncoding(cetJSON);
				}

				byte[] outputB = crypto.sign(inputB, true);

				System.out.println("Signature: " + new String(outputB));
			} else {
				if (compact) {
					crypto.setInputEncoding(cetCompact);
				} else {
					crypto.setInputEncoding(cetJSON);
				}

				byte[] sigB = sig.getBytes();

				crypto.verifyDetached(inputB, sigB);

				switch (crypto.getSignatureValidationResult()) {
					case svtValid:
						System.out.println("Verification succeeded");
						break;
					case svtCorrupted:
						System.out.println("Verification corrupted");
						break;
					case svtFailure:
						System.out.println("Verification failed");
						break;
					default:
						System.out.println("Verification unknown");
						break;
				}
			}

			System.out.println();
			confirmExit();
		} catch (Exception ex) {
			displayError(ex);
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



