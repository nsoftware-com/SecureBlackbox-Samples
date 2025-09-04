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

public class jadessigner extends ConsoleDemo {
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
				"  jadessigner -- SecureBlackbox JAdESSigner Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  jadessigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n" +
				"             [-level sig_level] [-detached] [-compact] [-flattened]\n\n" +
				"DESCRIPTION\n" +
				"  This sample illustrates the use of JAdESSigner component for creating JWS/JAdES signature.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the signed file will be saved (Required).\n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -level        The level for JAdES signatures. Enter the corresponding number. Valid values:\n\n" +
				"                  1  - JASL_GENERIC\n" +
				"                  2  - JASL_BASELINE_B\n" +
				"                  3  - JASL_BASELINE_T\n" +
				"                  4  - JASL_BASELINE_LT\n" +
				"                  5  - JASL_BASELINE_LTA\n\n" +
				"  -detached     Whether a detached signature should be produced.\n\n" +
				"  -compact      Whether the JWS compact serialization should be used.\n\n" +
				"  -flattened    Whether the flattened signature should be created.\n\n" +
				"EXAMPLES\n" +
				"  jadessigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword -flattened\n\n" +
				"  jadessigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"             -level 1 -detached -compact\n"
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

		JAdESSigner signer = new JAdESSigner();
		CertificateManager cm = new CertificateManager();

		try {
			if (optext(args, "-input")) {
				signer.setDataFile(optval(args, "-input"));
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
			} else {
				displayHelp("-cert is required.");
				return;
			}

			// Additional options
			if (optext(args, "-level")){
				signer.getNewSignature().setLevel(Integer.parseInt(optval(args, "-level")));
			} else{
				signer.getNewSignature().setLevel(JAdESSignature.jaslBaselineB);
			}

			if (optext(args, "-detached")) {
				signer.setDetached(true);
			}
			else {
				signer.setDetached(false);
			}

			if (optext(args, "-compact")) {
				signer.setCompactForm(true);
			}
			else {
				signer.setCompactForm(false);
			}

			if (optext(args, "-flattened")) {
				signer.setFlattenedSignature(true);
			}
			else {
				signer.setFlattenedSignature(false);
			}

			signer.sign();

			System.out.println("JWS/JAdES signature successfully created.\n");

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



