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

public class ftpclient extends ConsoleDemo {
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
            "  ftpclient -- SecureBlackbox FTPClient Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  ftpclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n" +
            "DESCRIPTION\n" +
            "  This sample illustrates basic FTP client operations.\n\n" +
            "  The options are as follows:\n\n" +
            "  host        The local host of serve (Required).\n\n" +
            "  port        The port of server (Required).\n\n" +
            "  -username   The user identifier to use for login.\n\n" +
            "  -password   The password to log in.\n\n" +
            "  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
            "EXAMPLES\n" +
            "  ftpclient localhost 21\n\n" +
            "  ftpclient localhost 12345 -username testuser -password pass -tlsmode implicit\n"
    );

    if (errMes.length() > 0) {
      System.out.println("Error: " + errMes);
      System.out.println();
    }

    confirmExit();
  }

  private static void displayCommands() {
    System.out.println("?         cd        man       quit    \n" +
                       "ascii     get       mkdir     rm      \n" +
                       "binary    help      put       rmdir   \n" +
                       "bye       ls        pwd       verbose");
  }

  private static void confirmExit() {
    System.out.println("Press Enter to exit the demo.");
    input();
  }

  static Boolean verbose = false;

  public static void main(String[] args) {
    if (args.length < 2) {
      displayHelp("host and port is required.");
      return;
    }

    final FTPClient client = new FTPClient();
    String username = "";

    try {
      client.addFTPClientEventListener(new FTPClientEventListener() {
        @Override
        public void controlReceive(FTPClientControlReceiveEvent e) {
          if (verbose) System.out.println(e.textLine);
        }

        @Override
        public void controlSend(FTPClientControlSendEvent e) {
          if (verbose) System.out.println(e.textLine);
        }

        @Override
        public void error(FTPClientErrorEvent e) {

        }

        @Override
        public void externalSign(FTPClientExternalSignEvent e) {

        }

        @Override
        public void fileNameChangeNeeded(FTPClientFileNameChangeNeededEvent e) {

        }

        @Override
        public void fileOperation(FTPClientFileOperationEvent e) {

        }

        @Override
        public void fileOperationResult(FTPClientFileOperationResultEvent e) {

        }

        @Override
        public void listEntry(FTPClientListEntryEvent e) {
          System.out.println("  " + client.getCurrentListEntry().getUnparsedName());
        }

        @Override
        public void notification(FTPClientNotificationEvent e) {

        }

        @Override
        public void progress(FTPClientProgressEvent e) {

        }

        @Override
        public void textDataLine(FTPClientTextDataLineEvent e) {

        }

        @Override
        public void TLSCertNeeded(FTPClientTLSCertNeededEvent e) {

        }

        @Override
        public void TLSCertValidate(FTPClientTLSCertValidateEvent e) {
          e.accept = true;
        }

        @Override
        public void TLSEstablished(FTPClientTLSEstablishedEvent e) {

        }

        @Override
        public void TLSHandshake(FTPClientTLSHandshakeEvent e) {

        }

        @Override
        public void TLSPSK(FTPClientTLSPSKEvent e) {

        }

        @Override
        public void TLSShutdown(FTPClientTLSShutdownEvent e) {

        }

        @Override
        public void supercoreIntercept(FTPClientSupercoreInterceptEvent e) {

        }
      });

      client.getTLSSettings().setAutoValidateCertificates(false);

      if (optext(args, "-username")) {
        username = optval(args, "-username");
        client.setUsername(username);
      }

      if (optext(args, "-password")) {
        client.setPassword(optval(args, "-password"));
      }

      if (optext(args, "-tlsmode")) {
        String tlsmode = optval(args, "-tlsmode");
        if (tlsmode.equalsIgnoreCase("none")) {
          client.getTLSSettings().setTLSMode(TLSSettings.smNoTLS);
        } else if (tlsmode.equalsIgnoreCase("explicit")) {
          client.getTLSSettings().setTLSMode(TLSSettings.smExplicitTLS);
        } else if (tlsmode.equalsIgnoreCase("implicit")) {
          client.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);
        }
      }

      System.out.println("Connecting to ftp://" + username + "@" + args[0] + ":" + args[1]);

      client.connect(args[0], Integer.parseInt(args[1]));

      // main loop to check for commands
      while (true) {
        String[] commands = translateCommandline(prompt("ftp", ">"));

        if (commands.length > 0) {
          if (commands[0].equalsIgnoreCase("ascii")) {
            client.setTransferType(FTPClient.cttText);
            System.out.println("Transfer mode text.");
          } else if (commands[0].equalsIgnoreCase("binary")) {
            client.setTransferType(FTPClient.cttBinary);
            System.out.println("Transfer mode binary.");
          } else if (commands[0].equalsIgnoreCase("put")) {
            if (commands.length < 3) {
              System.out.println("You need to pass two parameters: <local file> and <remote file>");
            } else {
              client.uploadFile(commands[1], commands[2]);
              System.out.println("Uploaded file: " + commands[1] + " -> " + commands[2]);
            }
          } else if (commands[0].equalsIgnoreCase("get")) {
            if (commands.length < 3) {
              System.out.println("You need to pass two parameters: <remote file> and <local file>");
            } else {
              client.downloadFile(commands[1], commands[2]);
              System.out.println("Downloaded file: " + commands[1] + " -> " + commands[2]);
            }
          } else if (commands[0].equalsIgnoreCase("cd")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <new dir> parameter");
            } else {
              client.changeDir(commands[1]);
              System.out.println("Changed directory: " + commands[1]);
            }
          } else if (commands[0].equalsIgnoreCase("mkdir")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <new dir> parameter");
            } else {
              client.makeDir(commands[1]);
              System.out.println("Created directory: " + commands[1]);
            }
          } else if (commands[0].equalsIgnoreCase("pwd")) {
            System.out.println("Current directory: " + client.getCurrentDir());
          } else if (commands[0].equalsIgnoreCase("ls")) {
            System.out.println("Listing " + client.getCurrentDir());
            client.listDir(true, true);
          } else if (commands[0].equalsIgnoreCase("rm")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <file name> parameter");
            } else {
              client.deleteFile(commands[1]);
              System.out.println("Deleted file: " + commands[1]);
            }
          } else if (commands[0].equalsIgnoreCase("rmdir")) {
            if (commands.length < 2) {
              System.out.println("You need to pass <dir name> parameter");
            } else {
              client.deleteDir(commands[1]);
              System.out.println("Deleted directory: " + commands[1]);
            }
          } else if (commands[0].equalsIgnoreCase("verbose")) {
            if (verbose) {
              verbose = false;
              System.out.println("Verbose mode off.");
            } else {
              verbose = true;
              System.out.println("Verbose mode on.");
            }
          } else if (commands[0].equalsIgnoreCase("bye") || commands[0].equalsIgnoreCase("exit") || commands[0].equalsIgnoreCase("quit")) {
            client.disconnect();
            break;
          } else if (commands[0].equalsIgnoreCase("?") || commands[0].equalsIgnoreCase("help") || commands[0].equalsIgnoreCase("man")) {
            displayCommands();
          } else {
            System.out.println("Command not recognized. Choose from these:\n");
            displayCommands();
          }
        } else {
          System.out.println("Command not recognized. Choose from these:\n");
          displayCommands();
        }
      }

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



