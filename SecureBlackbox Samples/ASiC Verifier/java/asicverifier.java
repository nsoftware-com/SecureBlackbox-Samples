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

import static secureblackbox.ASiCSignature.*;

public class asicverifier extends ConsoleDemo {
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
                "  asicverifier -- SecureBlackbox ASiCVerifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  asicverifier <-input input_file> [-extractpath extract_path]\n\n" +
                "DESCRIPTION\n" +
                "  ASiCVerifier demonstrates the usage of ASiCVerifier from SecureBlackbox.\n" +
                "  Used to verify the signature of and optionally extract any files in an Associated Signature Container (ASic).\n\n" +
                "  The options are as follows:\n\n" +
                "  -input         The ASiC to verify (Required).\n\n" +
                "  -extractpath   The path to extract files to. If unspecified, files will not be extracted.\n\n" +
                "EXAMPLES\n" +
                "  asicverifier -input C:\\asic\\myasic.scs\n"
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

    private static String translateSigType(int type) {
        switch (type) {
            case castCAdES:
                return "CAdES";
            case castXAdES:
                return "XAdES";
            case castTimestamp:
                return "Timestamp";
            default:
                return "Unknown";
        }
    }

    private static String translateValidationResult(int res) {
        switch (res) {
            case svtValid:
                return "The signature is valid.";
            case svtUnknown:
                return "Signature validity is unknown.";
            case svtCorrupted:
                return "The signature is corrupted.";
            case svtSignerNotFound:
                return "Failed to acquire the signing certificate. The signature cannot be validated.";
            case svtFailure:
                return "General failure.";
            default:
                return "Signature validity is unknown.";
        }
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            displayHelp("");
            return;
        }

        ASiCVerifier verifier = new ASiCVerifier();
        String input;

        if (optext(args, "-input")) {
            input = optval(args, "-input");
        } else {
            displayHelp("-input is required.");
            return;
        }

        try {
            verifier.setInputFile(input);

            if (optext(args, "-extractpath")) {
                verifier.setExtractionMode(secureblackbox.ASiCVerifier.aemAll);
                verifier.setOutputPath(optval(args, "-extractpath"));
            }

            verifier.verify();

            System.out.println("There are " + verifier.getSignatures().size() + " signatures in this file.");
            for (int i = 0; i < verifier.getSignatures().size(); i++) {
                System.out.println("Signature #" + (i + 1));
                System.out.println("  SignatureType: " + translateSigType(verifier.getSignatures().item(i).getSignatureType()));
                System.out.println("  File(s): " + verifier.getSignatures().item(i).getSignedFiles());

                System.out.println("  Validation Result: " + verifier.getSignatures().item(i).getSignatureValidationResult() + ", " + translateValidationResult(verifier.getSignatures().item(i).getSignatureValidationResult()));
                System.out.println("  Chain Result: " + verifier.getSignatures().item(i).getChainValidationResult() + "\n");
            }

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



