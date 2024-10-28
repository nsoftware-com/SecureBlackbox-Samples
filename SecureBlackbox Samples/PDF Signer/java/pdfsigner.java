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

public class pdfsigner extends ConsoleDemo {
	private static String optval(String[] args, String option) {
		for (int x = 0; x < args.length - 1; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return args[x + 1];
			}
		}
		return "";
	}

	private static boolean optext(String[] args, String option) {
		for (int x = 0; x < args.length; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return true;
			}
		}
		return false;
	}

	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
				"  pdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  pdfsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n" +
				"             [-level sig_level] [-hashalg hashalg] [-author author] [-reason reason] [-signame signame]\n\n" +
				"DESCRIPTION\n" +
				"  This sample illustrates the use of PDFSigner component for signing PDF documents.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the signed file will be saved (Required).\n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -level        The level for PAdES signatures. Enter the corresponding number. Valid values:\n\n" +
				"                  0  - PASL_LEGACY\n" +
				"                  1  - PASL_BASELINE_B\n" +
				"                  2  - PASL_BASELINE_T\n" +
				"                  3  - PASL_BASELINE_LT\n" +
				"                  4  - PASL_BASELINE_LTA\n" +
				"                  5  - PASL_BES\n" +
				"                  6  - PASL_EPES\n" +
				"                  7  - PASL_LTV\n\n" +
				"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224\n\n" +
				"  -author       The name of the signer who produced this signature.\n\n" +
				"  -reason       Specifies the reason of the signing, for example to confirm the document correctness.\n\n" +
				"  -signame      Specifies the signature identifier in the PDF-file.\n\n" +
				"EXAMPLES\n" +
				"  pdfsigner -input C:\\helloworld.pdf -output C:\\sign.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n" +
				"  pdfsigner -input C:\\helloworld.pdf -output C:\\sign.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"           -hashalg SHA256 -level 5 -author \"Test author\"\n"
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

		PDFSigner signer = new PDFSigner();
		CertificateManager cm = new CertificateManager();

		try {
			if (optext(args, "-input")) {
				signer.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-output")) {
				signer.setOutputFile(optval(args, "-output"));
			} else {
				displayHelp("-output is required.");
				return;
			}

			if (optext(args, "-cert")) {
				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				signer.setSigningCertificate(cm.getCertificate());
				if (cm.getCertificate().getKeyAlgorithm() == "id-dsa") {
					signer.getNewSignature().setHashAlgorithm("SHA1");
				}
			} else {
				displayHelp("-cert is required.");
				return;
			}

			// Additional options
			if (optext(args, "-hashalg")) {
				signer.getNewSignature().setHashAlgorithm(optval(args, "-hashalg"));
			}
			if (optext(args, "-level")) {
				signer.getNewSignature().setLevel(Integer.parseInt(optval(args, "-level")));
			}
			if (optext(args, "-author")) {
				signer.getNewSignature().setAuthorName(optval(args, "-author"));
			}
			if (optext(args, "-reason")) {
				signer.getNewSignature().setReason(optval(args, "-reason"));
			}
			if (optext(args, "-signame")) {
				signer.getNewSignature().setSignatureName(optval(args, "-signame"));
			}

			signer.sign();

			System.out.println("The document was signed successfully.\n");

			confirmExit();
		} catch (Exception ex) {
			ConsoleDemo.displayError(ex);
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



