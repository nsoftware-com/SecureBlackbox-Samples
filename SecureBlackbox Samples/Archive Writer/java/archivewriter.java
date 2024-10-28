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

public class archivewriter extends ConsoleDemo {
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
                "  archivewriter -- SecureBlackbox ArchiveWriter Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  archivewriter <-arctype archive_type> <-input input1 input2 ....> <-output output_file> [-pass encryption_password] [-enctype encryption_type]\n\n" +
                "DESCRIPTION\n" +
                "  ArchiveWriter demonstrates the usage of ArchiveWriter from SecureBlackbox.\n" +
                "  Used to create archive.\n\n" +
                "  The options are as follows:\n\n" +
                "  -arctype      The type of archive (Required). Valid values:\n\n" +
                "                  1 - AFT_ZIP\n" +
                "                  2 - AFT_GZIP\n" +
                "                  3 - AFT_BZIP_2\n" +
                "                  4 - AFT_TAR\n" +
                "                  5 - AFT_TAR_GZIP\n" +
                "                  6 - AFT_TAR_BZIP_2\n\n" +
                "  -input        List of files or directories compressing to archive (Required).\n\n" +
                "  -output       Where the archive file will be saved (Required).\n\n" +
                "  -pass         The password for the encryption.\n\n" +
                "  -enctype      The encryption type. Valid values: \n\n" +
                "                  0 - AET_DEFAULT\n" +
                "                  1 - AET_NO_ENCRYPTION\n" +
                "                  2 - AET_GENERIC\n" +
                "                  3 - AET_WIN_ZIP\n" +
                "                  4 - AET_STRONG\n\n" +
                "EXAMPLES\n" +
                "  archivewriter -arctype 1 -input C:\\archive\\helloworld.txt -output C:\\archive\\myarchive.zip \n\n" +
                "  archivewriter -arctype 5 -input C:\\archive\\helloworld.txt C:\\archive\\temp -output C:\\archive\\myarchive.tar -enctype 2 -pass mypassword \n"
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

        ArchiveWriter archive = new ArchiveWriter();
        int arctype;
        String output;

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

        if (!optext(args, "-input")) {
            displayHelp("-input is required.");
            return;
        }

        if (optext(args, "-output")) {
            output = optval(args, "-output");
        } else {
            displayHelp("-output is required.");
            return;
        }

        try {
            // Additional options
            if (optext(args, "-enctype")) {
                archive.setEncryptionType(Integer.parseInt(optval(args, "-enctype")));
                archive.setEncryptionPassword(optval(args, "-pass"));
            }

            // Create archive
            archive.createNew(arctype);

            // Add input files
            for (int x = 0; x < args.length; x++)
                if (args[x].equalsIgnoreCase("-input") && x != args.length - 1) {
                    for (int i = x + 1; i < args.length; i++) {
                        if (args[i].startsWith("-")) break;

                        File f = new File(args[i]);
                        if (f.exists()) // file exists
                        {
                            archive.addFile(f.getName(), args[i]);
                        } else {
                            archive.addFiles("", args[i], true);
                        }
                    }

                    break;
                }

            System.out.println("List of files added to the archive");
            for (int x = 0; x < archive.getFiles().size(); x++) {
                System.out.println("    " + archive.getFiles().item(x).getPath());
            }
            System.out.println("");

            // Save archive
            archive.save(output);

            System.out.println("Archive created.\n");

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



