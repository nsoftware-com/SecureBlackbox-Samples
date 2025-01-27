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

public class archivereader extends ConsoleDemo {
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
                        "  archivereader -- SecureBlackbox ArchiveReader Demo Application\n\n" +
                        "SYNOPSIS\n" +
                        "  archivereader <-arctype archive_type> <-input input_file> [-output output_path] [-pass decryption_password]\n\n" +
                        "DESCRIPTION\n" +
                        "  ArchiveReader demonstrates the use of ArchiveReader from SecureBlackbox.\n" +
                        "  Use this component to extract files from existing archives.\n\n" +
                        "  The options are as follows:\n\n" +
                        "  -arctype      The type of archive (Required). Valid values:\n\n" +
                        "                  1 - AFT_ZIP\n" +
                        "                  2 - AFT_GZIP\n" +
                        "                  3 - AFT_BZIP_2\n" +
                        "                  4 - AFT_TAR\n" +
                        "                  5 - AFT_TAR_GZIP\n" +
                        "                  6 - AFT_TAR_BZIP_2\n\n" +
                        "  -input        An input archive file (Required).\n\n" +
                        "  -output       The directory to save extracted files to.\n\n" +
                        "  -pass         A password to decrypt the encrypted archive.\n\n" +
                        "EXAMPLES\n" +
                        "  archivereader -arctype 1 -input C:\\archive\\helloworld.zip -output C:\\archive \n\n" +
                        "  archivereader -arctype 5 -input C:\\archive\\helloworld.tar -pass mypassword \n"
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

        ArchiveReader archive = new ArchiveReader();
        String input;
        int arctype;

        if (optext(args, "-arctype")) {
            try {
                arctype = Integer.parseInt(optval(args, "-arctype"));
            } catch (NumberFormatException e) {
                displayHelp("-arctype invalid value.");
                return;
            }
        } else {
            displayHelp("-arctype is required.");
            return;
        }

        if (optext(args, "-input")) {
            input = optval(args, "-input");
        } else {
            displayHelp("-input is required.");
            return;
        }

        // Open archive
        try {
            // Additional options
            archive.setDecryptionPassword(optval(args, "-pass"));

            archive.open(arctype, input);

            System.out.println("The following files were found in the archive:");
            for (int x = 0; x < archive.getFiles().size(); x++) {
                System.out.println("    " + archive.getFiles().item(x).getFileName());
            }
            System.out.println("");

            if (optext(args, "-output")) {
                // Extract all files
                archive.setOverwrite(true);
                archive.extractAll(optval(args, "-output"));

                System.out.println("All files from the archive have been extracted.\n");
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



