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

public class simplepdfsigner extends ConsoleDemo {
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
						"  simplepdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  simplepdfsigner <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password]\n" +
						"             [-pkcs11 pkcs11_file] [-pin pkcs11_pin] [-win32 win32_name]\n\n" +
						"DESCRIPTION\n" +
						"  PDFSigner demonstrates the usage of PDFSigner from SecureBlackbox.\n" +
						"  This sample illustrates the use of PDFSigner component for signing PDF documents. \n\n" +
						"  The options are as follows:\n\n" +
						"  -input        An input file to sign (Required).\n\n" +
						"  -output       Where the signed file will be saved (Required).\n\n" +
						"  -cert         The certificate used to sign files.\n\n" +
						"  -certpass     The password for the signing certificate.\n\n" +
						"  -pkcs11       The pkcs11 storage used to sign file.\n\n" +
						"  -pin          The PIN for pkcs11 storage\n\n" +
						"  -win32        The win32 store name\n\n" +
						"EXAMPLES\n" +
						"  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n" +
						"  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -pkcs11 C:\\pkcs11\\pkcs11.dll -pin mypassword \n\n" +
						"  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -win32 My \n"
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
		CertificateStorage certstorage = new CertificateStorage();
		try {
			String certFile = "";
			String certPass = "";
			String pkcs11File = "";
			String pin = "";
			String win32Store = "My";

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
				certFile = optval(args, "-cert");
			}

			if (optext(args, "-certpass")) {
				certPass = optval(args, "-certpass");
			}

			if (optext(args, "-pkcs11")) {
				pkcs11File = optval(args, "-pkcs11");
			}

			if (optext(args, "-pin")) {
				pin = optval(args, "-pin");
			}

			if (optext(args, "-win32")) {
				win32Store = optval(args, "-win32");
			}

			if (certFile.isEmpty() && pkcs11File.isEmpty() && win32Store.isEmpty()) {
				displayHelp("-cert or -pkcs11 or -win32 is required.");
				return;
			}

			if (!certFile.isEmpty() && !pkcs11File.isEmpty() && !win32Store.isEmpty()) {
				displayHelp("Use only one -cert or -pkcs11 or -win32 parameter.");
				return;
			}

			if (!certFile.isEmpty()) {
				CertificateManager cm = new CertificateManager();
				cm.importFromFile(certFile, certPass);
				signer.setSigningCertificate(cm.getCertificate());
			} else {
				try {
					if (!pkcs11File.isEmpty()) {
						certstorage.open("pkcs11://user:" + pin + "@/" + pkcs11File);
					} else {
						certstorage.open("system://?store=" + win32Store);
					}

					signer.setSigningCertificate(certstorage.getCertificates().item(0));
				} catch (Exception ex) {
					System.out.println("Error: Cannot load certificate!");
					return;
				}
			}

			signer.getNewSignature().setLevel(PDFSignature.paslBES);
			signer.getWidget().setInvisible(false);
			signer.setIgnoreChainValidationErrors(true);

			signer.sign();

			System.out.println("PDF file successfully signed.\n");

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



