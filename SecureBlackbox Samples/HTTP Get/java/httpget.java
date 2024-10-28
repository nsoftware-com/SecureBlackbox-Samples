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

public class httpget extends ConsoleDemo {
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
                    "  httpget -- SecureBlackbox HTTPClient Demo Application\n\n" +
                    "SYNOPSIS\n" +
                    "  httpget <url>\n\n" +
                    "DESCRIPTION\n" +
                    "  This sample illustrates the ways of making GET requests with HTTPSClient.\n\n" +
                    "  The options are as follows:\n\n" +
                    "  url       The local host of server (Required).\n\n" +
                    "EXAMPLES\n" +
                    "  httpget https://www.google.com\n"
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
    if (args.length < 1) {
      displayHelp("");
      return;
    }

    HTTPClient client = new HTTPClient();

    try {
      client.addHTTPClientEventListener(new HTTPClientEventListener() {
        @Override
        public void cookie(HTTPClientCookieEvent e) {

        }

        @Override
        public void documentBegin(HTTPClientDocumentBeginEvent e) {

        }

        @Override
        public void documentEnd(HTTPClientDocumentEndEvent e) {

        }

        @Override
        public void dynamicDataNeeded(HTTPClientDynamicDataNeededEvent e) {

        }

        @Override
        public void error(HTTPClientErrorEvent e) {

        }

        @Override
        public void externalSign(HTTPClientExternalSignEvent e) {

        }

        @Override
        public void headersPrepared(HTTPClientHeadersPreparedEvent e) {

        }

        @Override
        public void headersReceived(HTTPClientHeadersReceivedEvent e) {

        }

        @Override
        public void notification(HTTPClientNotificationEvent e) {

        }

        @Override
        public void progress(HTTPClientProgressEvent e) {

        }

        @Override
        public void redirection(HTTPClientRedirectionEvent e) {

        }

        @Override
        public void TLSCertNeeded(HTTPClientTLSCertNeededEvent e) {

        }

        @Override
        public void TLSCertValidate(HTTPClientTLSCertValidateEvent e) {
          e.accept = true;
        }

        @Override
        public void TLSEstablished(HTTPClientTLSEstablishedEvent e) {

        }

        @Override
        public void TLSHandshake(HTTPClientTLSHandshakeEvent e) {

        }

        @Override
        public void TLSPSK(HTTPClientTLSPSKEvent e) {

        }

        @Override
        public void TLSShutdown(HTTPClientTLSShutdownEvent e) {

        }

        @Override
        public void supercoreIntercept(HTTPClientSupercoreInterceptEvent e) {

        }
      });

      client.get(args[0]);

      System.out.println("Server Response:\n\n" + client.getOutputString() + "\n");

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



