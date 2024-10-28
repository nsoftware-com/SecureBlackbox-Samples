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
import java.util.ArrayList;
import java.util.StringTokenizer;
import secureblackbox.*;

public class pgpkeys extends ConsoleDemo {
  public static String[] translateCommandline(String toProcess) {
    if (toProcess == null || toProcess.length() == 0) {
      return new String[0];
    }

    final int normal = 0;
    final int inQuote = 1;
    final int inDoubleQuote = 2;
    int state = normal;
    final StringTokenizer tok = new StringTokenizer(toProcess, "\"\' ", true);
    final ArrayList<String> result = new ArrayList<String>();
    final StringBuilder current = new StringBuilder();
    boolean lastTokenHasBeenQuoted = false;

    while (tok.hasMoreTokens()) {
      String nextTok = tok.nextToken();
      switch (state) {
        case inQuote:
          if ("\'".equals(nextTok)) {
            lastTokenHasBeenQuoted = true;
            state = normal;
          } else {
            current.append(nextTok);
          }
          break;
        case inDoubleQuote:
          if ("\"".equals(nextTok)) {
            lastTokenHasBeenQuoted = true;
            state = normal;
          } else {
            current.append(nextTok);
          }
          break;
        default:
          if ("\'".equals(nextTok)) {
            state = inQuote;
          } else if ("\"".equals(nextTok)) {
            state = inDoubleQuote;
          } else if (" ".equals(nextTok)) {
            if (lastTokenHasBeenQuoted || current.length() != 0) {
              result.add(current.toString());
              current.setLength(0);
            }
          } else {
            current.append(nextTok);
          }
          lastTokenHasBeenQuoted = false;
          break;
      }
    }
    if (lastTokenHasBeenQuoted || current.length() != 0) {
      result.add(current.toString());
    }
    if (state == inQuote || state == inDoubleQuote) {
      throw new RuntimeException("unbalanced quotes in " + toProcess);
    }
    return result.toArray(new String[result.size()]);
  }

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
            "  pgpkeys -- SecureBlackbox PGPKeys Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  pgpkeys <-new/-open> [-sec secret_keys] [-pub public_keys]\n\n" +
            "DESCRIPTION\n" +
            "  This sample is a simple OpenPGP keyring manager. \n\n" +
            "  The options are as follows:\n\n" +
            "  -new        Whether new keys storage.\n\n" +
            "  -open       Whether open existing keys storage.\n\n" +
            "  -sec        The secret keys file.\n\n" +
            "  -pub        The public keys file.\n\n" +
            "EXAMPLES\n" +
            "  pgpkeys -new\n\n" +
            "  pgpkeys -open -sec C:\\secbbox.skr -pub C:\\secbbox.pkr\n"
    );

    if (errMes.length() > 0) {
      System.out.println("Error: " + errMes);
      System.out.println();
    }

    confirmExit();
  }

  private static void displayCommands() {
    System.out.println("PGPKeys Commands:\n" +
            "   list                           keys list\n" +
            "   new <username>                 generate new key\n" +
            "   add <key file>                 add key from file\n" +
            "   del <keyId>                    delete key\n" +
            "   export <keyId> <key file>      export key to file\n" +
            "   save <sec file> <pub file>     save keys to files\n" +
            "   exit, quit, q, bye             exit\n" +
            "   ?, help, man                   help");
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

    boolean newF = false;
    boolean openF = false;
    String seckey = "";
    String pubkey = "";

    if (optext(args, "-new")) {
      newF = true;
    }

    if (optext(args, "-open")) {
      openF = true;
    }

    if (!(newF || openF)) {
      displayHelp("-new or -open is required.");
      return;
    }

    if (newF && openF) {
      displayHelp("Use only one -new or -open parameter.");
      return;
    }

    if (optext(args, "-sec")) {
      seckey = optval(args, "-sec");
    }

    if (optext(args, "-pub")) {
      pubkey = optval(args, "-pub");
    }

    final PGPKeyring keyring = new PGPKeyring();

    try {
      if (openF) {
        if (seckey.length() == 0 && pubkey.length() == 0)  {
          displayHelp("-sec or -pub is required when use -open");
          return;
        }

        if (pubkey.length() > 0) {
          keyring.importFromFile(pubkey);
        }

        if (seckey.length() > 0) {
          keyring.importFromFile(seckey);
        }
      }
    } catch (Exception ex) {
      displayError(ex);
      return;
    }

    // main loop to check for commands
    while (true) {
      try {
        String[] commands = translateCommandline(prompt("pgp", ">"));

        if (commands.length > 0) {
          if (commands[0].equalsIgnoreCase("list")) {

            System.out.println("Keys:");

            for (int x = 0; x < keyring.getKeys().size(); x++) {
              if (!keyring.getKeys().item(x).getIsSubkey()) {
                System.out.println("  Key id: " + keyring.getKeys().item(x).getKeyID());
                System.out.println("  Key FP: " + keyring.getKeys().item(x).getKeyFP());
                System.out.println("  Algorithm: " + keyring.getKeys().item(x).getPublicKeyAlgorithm() + " (" + Integer.toString(keyring.getKeys().item(x).getBitsInKey()) + ")");
                System.out.println("  Created: " + keyring.getKeys().item(x).getTimestamp());
                System.out.println("  Valid to: " + keyring.getKeys().item(x).getValidTo());

                PGPKeyManager km = new PGPKeyManager();

                km.setPinnedKey(keyring.getKeys().item(x));
                km.importPinned();

                System.out.println("  Users (" + Integer.toString(km.getUsers().size()) + "):");
                for (int y = 0; y < km.getUsers().size(); y++) {
                  System.out.println("    Username: " + km.getUsers().item(y).getUsername());
                  System.out.println("    Signatures:");
                  for (int k = 0; k < km.getSignatures().size(); k++) {
                    if (km.getSignatures().item(k).getTarget().equals(km.getUsers().item(y).getUsername())) {
                      if (km.getSignatures().item(k).getRevocation()){
                        System.out.println("      Type: Revocation");
                      }
                      else {
                        System.out.println("      Type: Signature");
                      }

                      System.out.println("      Signer: " + km.getSignatures().item(k).getSignerKeyID());
                      System.out.println("      Created: " + km.getSignatures().item(k).getCreationTime());
                    }
                  }
                }

                System.out.println("  Subkeys (" + Integer.toString(km.getSubkeys().size()) + "):");
                for (int y = 0; y < km.getSubkeys().size(); y++) {
                  System.out.println("    Key id: " + km.getSubkeys().item(y).getKeyID());
                  System.out.println("    Key FP: " + km.getSubkeys().item(y).getKeyFP());
                  System.out.println("    Algorithm: " + km.getSubkeys().item(y).getPublicKeyAlgorithm() + " (" + Integer.toString(km.getSubkeys().item(y).getBitsInKey()) + ")");
                  System.out.println("    Created: " + km.getSubkeys().item(y).getTimestamp());
                  System.out.println("    Valid to: " + km.getSubkeys().item(y).getValidTo());

                  System.out.println("    Signatures:");
                  for (int k = 0; k < km.getSignatures().size(); k++) {
                    if (km.getSignatures().item(k).getTarget().equals(km.getSubkeys().item(y).getKeyID())) {
                      if (km.getSignatures().item(k).getRevocation()){
                        System.out.println("      Type: Revocation");
                      }
                      else {
                        System.out.println("      Type: Signature");
                      }

                      System.out.println("      Signer: " + km.getSignatures().item(k).getSignerKeyID());
                      System.out.println("      Created: " + km.getSignatures().item(k).getCreationTime());
                    }
                  }
                }
                System.out.println();
              }
            }
          } else if (commands[0].equalsIgnoreCase("new")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <username> parameter");
            } else {
              PGPKeyManager km = new PGPKeyManager();

              km.generatePair(6, commands[1], 0, "", "");

              keyring.setPinnedKey(km.getKey());
              keyring.importPinned();

              System.out.println("Generate new PGP key pair");
            }
          } else if (commands[0].equalsIgnoreCase("add")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <key file> parameter");
            } else {
              keyring.importFromFile(commands[1]);

              System.out.println("Added key from file: " + commands[1]);
            }
          } else if (commands[0].equalsIgnoreCase("del")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <keyId> parameter");
            } else {
              keyring.removeByID(commands[1]);

              System.out.println("Key was successfully removed.");
            }
          } else if (commands[0].equalsIgnoreCase("export")) {
            if (commands.length < 3) {
              System.out.println("You need to pass two parameters: <keyId> and <key file>");
            } else {
              int idx = -1;
              for (int x = 0; x < keyring.getKeys().size(); x++) {
                if (!keyring.getKeys().item(x).getIsSubkey() && keyring.getKeys().item(x).getKeyID().equalsIgnoreCase(commands[1])) {
                  idx = x;
                  break;
                }
              }

              if (idx >= 0) {
                PGPKeyManager km = new PGPKeyManager();
                km.setPinnedKey(keyring.getKeys().item(idx));
                km.importPinned();

                km.exportToFile(commands[2], false);

                System.out.println("Export public key to file: " + commands[2]);
              }
              else {
                System.out.println("Key with Id not found.");
              }
            }
          } else if (commands[0].equalsIgnoreCase("save")) {
            if (commands.length < 3) {
              System.out.println("You need to pass two parameters: <sec file> and <pub file>");
            } else {
              if (commands[1].length() > 0) {
                keyring.exportToFile(commands[1], true);
              }

              if (commands[2].length() > 0) {
                keyring.exportToFile(commands[2], false);
              }

              System.out.println("Save keys to files: " + commands[1] + " and " + commands[2]);
            }
          } else if (commands[0].equalsIgnoreCase("bye") || commands[0].equalsIgnoreCase("exit") || commands[0].equalsIgnoreCase("quit") || commands[0].equalsIgnoreCase("q")) {
            System.out.println();
            break;
          } else if (commands[0].equalsIgnoreCase("?") || commands[0].equalsIgnoreCase("help") || commands[0].equalsIgnoreCase("man")) {
            displayCommands();
          } else {
            System.out.println("Command not recognized.");
            displayCommands();
          }
        } else {
          System.out.println("Command not recognized.");
          displayCommands();
        }
      } catch (Exception ex) {
        System.out.println("Error: " + ex.getMessage());
      }
    }

    confirmExit();
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
      System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
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



