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

public class kmipserver extends ConsoleDemo {
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
            "  kmipserver -- SecureBlackbox KMIPServer Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  kmipserver <listening_port> <-storage storage_file> [-cert certificate_file] [-certpass certificate_password]\n\n" +
            "          [-users users_file] [-userspass users_password] [-tlsmode tlsmode] [-usehttp]\n\n" +
            "DESCRIPTION\n" +
            "  KMIPServer demonstrates the usage of KMIPServer from SecureBlackbox.\n" +
            "  The options are as follows:\n\n" +
            "  -storage        The storage used in kmip server (Required).\n\n" +
            "  -cert           The certificate used in kmip server as CA certificate.\n\n" +
            "  -certpass       The password for the certificate.\n\n" +
            "  -users          The users used in kmip server.\n\n" +
            "  -userspass      The password for the users file.\n\n" +
            "  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, implicit.\n\n" +
            "  -usehttp        Whether to use http.\n\n" +
            "EXAMPLES\n" +
            "  kmipserver 80 -storage C:\\storage.tmp\n\n" +
            "  kmipserver 8080 -storage C:\\storage.tmp -cert C:\\certs\\mycert.pfx -certpass mypassword -usehttp\n"
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
      displayHelp("listening_port is required.");
      return;
    }

    final KMIPServer server = new KMIPServer();
    try {
      server.setPort(Integer.parseInt(args[0]));
    } catch (Exception ex) {
      displayHelp("Invalid port value");
      return;
    }

    try {
      server.addKMIPServerEventListener(new KMIPServerEventListener() {
        @Override
        public void accept(KMIPServerAcceptEvent e) {

        }

        @Override
        public void activateObject(KMIPServerActivateObjectEvent e) {

        }

        @Override
        public void add(KMIPServerAddEvent e) {

        }

        @Override
        public void addKey(KMIPServerAddKeyEvent e) {

        }

        @Override
        public void afterAdd(KMIPServerAfterAddEvent e) {

        }

        @Override
        public void afterAddKey(KMIPServerAfterAddKeyEvent e) {

        }

        @Override
        public void afterBrowse(KMIPServerAfterBrowseEvent e) {

        }

        @Override
        public void afterDecrypt(KMIPServerAfterDecryptEvent e) {

        }

        @Override
        public void afterDeriveKey(KMIPServerAfterDeriveKeyEvent e) {

        }

        @Override
        public void afterEdit(KMIPServerAfterEditEvent e) {

        }

        @Override
        public void afterEncrypt(KMIPServerAfterEncryptEvent e) {

        }

        @Override
        public void afterGenerate(KMIPServerAfterGenerateEvent e) {

        }

        @Override
        public void afterGenerateKey(KMIPServerAfterGenerateKeyEvent e) {

        }

        @Override
        public void afterGenerateKeyPair(KMIPServerAfterGenerateKeyPairEvent e) {

        }

        @Override
        public void afterHash(KMIPServerAfterHashEvent e) {

        }

        @Override
        public void afterList(KMIPServerAfterListEvent e) {

        }

        @Override
        public void afterObtainLease(KMIPServerAfterObtainLeaseEvent e) {

        }

        @Override
        public void afterReadObject(KMIPServerAfterReadObjectEvent e) {

        }

        @Override
        public void afterReCertify(KMIPServerAfterReCertifyEvent e) {

        }

        @Override
        public void afterReKey(KMIPServerAfterReKeyEvent e) {

        }

        @Override
        public void afterRekeyKeyPair(KMIPServerAfterRekeyKeyPairEvent e) {

        }

        @Override
        public void afterRemoveObject(KMIPServerAfterRemoveObjectEvent e) {

        }

        @Override
        public void afterSign(KMIPServerAfterSignEvent e) {

        }

        @Override
        public void afterVerify(KMIPServerAfterVerifyEvent e) {

        }

        @Override
        public void afterVerifyHash(KMIPServerAfterVerifyHashEvent e) {

        }

        @Override
        public void archiveObject(KMIPServerArchiveObjectEvent e) {

        }

        @Override
        public void authAttempt(KMIPServerAuthAttemptEvent e) {

        }

        @Override
        public void beforeAdd(KMIPServerBeforeAddEvent e) {

        }

        @Override
        public void beforeAddKey(KMIPServerBeforeAddKeyEvent e) {

        }

        @Override
        public void beforeBrowse(KMIPServerBeforeBrowseEvent e) {

        }

        @Override
        public void beforeDecrypt(KMIPServerBeforeDecryptEvent e) {

        }

        @Override
        public void beforeDeriveKey(KMIPServerBeforeDeriveKeyEvent e) {

        }

        @Override
        public void beforeEdit(KMIPServerBeforeEditEvent e) {

        }

        @Override
        public void beforeEncrypt(KMIPServerBeforeEncryptEvent e) {

        }

        @Override
        public void beforeGenerate(KMIPServerBeforeGenerateEvent e) {

        }

        @Override
        public void beforeGenerateKey(KMIPServerBeforeGenerateKeyEvent e) {

        }

        @Override
        public void beforeGenerateKeyPair(KMIPServerBeforeGenerateKeyPairEvent e) {

        }

        @Override
        public void beforeHash(KMIPServerBeforeHashEvent e) {

        }

        @Override
        public void beforeList(KMIPServerBeforeListEvent e) {

        }

        @Override
        public void beforeObtainLease(KMIPServerBeforeObtainLeaseEvent e) {

        }

        @Override
        public void beforeReadObject(KMIPServerBeforeReadObjectEvent e) {

        }

        @Override
        public void beforeReCertify(KMIPServerBeforeReCertifyEvent e) {

        }

        @Override
        public void beforeReKey(KMIPServerBeforeReKeyEvent e) {

        }

        @Override
        public void beforeRekeyKeyPair(KMIPServerBeforeRekeyKeyPairEvent e) {

        }

        @Override
        public void beforeRemoveObject(KMIPServerBeforeRemoveObjectEvent e) {

        }

        @Override
        public void beforeSign(KMIPServerBeforeSignEvent e) {

        }

        @Override
        public void beforeVerify(KMIPServerBeforeVerifyEvent e) {

        }

        @Override
        public void beforeVerifyHash(KMIPServerBeforeVerifyHashEvent e) {

        }

        @Override
        public void cancel(KMIPServerCancelEvent e) {

        }

        @Override
        public void check(KMIPServerCheckEvent e) {

        }

        @Override
        public void connect(KMIPServerConnectEvent e) {

        }

        @Override
        public void decrypt(KMIPServerDecryptEvent e) {

        }

        @Override
        public void deleteAttribute(KMIPServerDeleteAttributeEvent e) {

        }

        @Override
        public void deriveKey(KMIPServerDeriveKeyEvent e) {

        }

        @Override
        public void disconnect(KMIPServerDisconnectEvent e) {

        }

        @Override
        public void encrypt(KMIPServerEncryptEvent e) {

        }

        @Override
        public void error(KMIPServerErrorEvent e) {
          System.out.println("Error code: " + Integer.toString(e.errorCode) + ". Description: " + e.description);
        }

        @Override
        public void externalSign(KMIPServerExternalSignEvent e) {

        }

        @Override
        public void generate(KMIPServerGenerateEvent e) {

        }

        @Override
        public void generateKey(KMIPServerGenerateKeyEvent e) {

        }

        @Override
        public void generateKeyPair(KMIPServerGenerateKeyPairEvent e) {

        }

        @Override
        public void getUsageAllocation(KMIPServerGetUsageAllocationEvent e) {

        }

        @Override
        public void hash(KMIPServerHashEvent e) {

        }

        @Override
        public void headersPrepared(KMIPServerHeadersPreparedEvent e) {

        }

        @Override
        public void KMIPAuthAttempt(KMIPServerKMIPAuthAttemptEvent e) {
          if (server.getUsers().size() == 0)
            e.accept = true;
        }

        @Override
        public void list(KMIPServerListEvent e) {

        }

        @Override
        public void listAttributes(KMIPServerListAttributesEvent e) {

        }

        @Override
        public void notification(KMIPServerNotificationEvent e) {

        }

        @Override
        public void obtainLease(KMIPServerObtainLeaseEvent e) {

        }

        @Override
        public void operationAttempt(KMIPServerOperationAttemptEvent e) {
          System.out.println("Request for " + e.operation + " from " + e.username);
          e.reject = false;
        }

        @Override
        public void poll(KMIPServerPollEvent e) {

        }

        @Override
        public void readAttribute(KMIPServerReadAttributeEvent e) {

        }

        @Override
        public void readObject(KMIPServerReadObjectEvent e) {

        }

        @Override
        public void reCertify(KMIPServerReCertifyEvent e) {

        }

        @Override
        public void recoverObject(KMIPServerRecoverObjectEvent e) {

        }

        @Override
        public void reKey(KMIPServerReKeyEvent e) {

        }

        @Override
        public void rekeyKeyPair(KMIPServerRekeyKeyPairEvent e) {

        }

        @Override
        public void removeObject(KMIPServerRemoveObjectEvent e) {

        }

        @Override
        public void request(KMIPServerRequestEvent e) {

        }

        @Override
        public void response(KMIPServerResponseEvent e) {

        }

        @Override
        public void revokeObject(KMIPServerRevokeObjectEvent e) {

        }

        @Override
        public void RNGGenerate(KMIPServerRNGGenerateEvent e) {

        }

        @Override
        public void RNGSeed(KMIPServerRNGSeedEvent e) {

        }

        @Override
        public void setAttribute(KMIPServerSetAttributeEvent e) {

        }

        @Override
        public void sign(KMIPServerSignEvent e) {

        }

        @Override
        public void TLSCertValidate(KMIPServerTLSCertValidateEvent e) {

        }

        @Override
        public void TLSEstablished(KMIPServerTLSEstablishedEvent e) {

        }

        @Override
        public void TLSHandshake(KMIPServerTLSHandshakeEvent e) {

        }

        @Override
        public void TLSPSK(KMIPServerTLSPSKEvent e) {

        }

        @Override
        public void TLSShutdown(KMIPServerTLSShutdownEvent e) {

        }

        @Override
        public void validateChain(KMIPServerValidateChainEvent e) {

        }

        @Override
        public void verify(KMIPServerVerifyEvent e) {

        }

        @Override
        public void verifyHash(KMIPServerVerifyHashEvent e) {

        }

        @Override
        public void supercoreIntercept(KMIPServerSupercoreInterceptEvent e) {

        }
      });

      if (optext(args, "-storage")) {
        server.setStorageFileName(optval(args, "-storage"));
      } else {
        displayHelp("-storage is required.");
        return;
      }

      if (optext(args, "-cert")) {
        CertificateManager cm = new CertificateManager();
        cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
        server.setCACertificate(cm.getCertificate());
      }

      if (optext(args, "-users")) {
        UserManager um = new UserManager();
        um.importFromFile(optval(args, "-users"), optval(args, "-userspass"), true);

        for (int x = 0; x < um.getUsers().size(); x++) {
          server.getUsers().add(um.getUsers().item(x));
        }
      }

      if (optext(args, "-tlsmode")) {
        String tlsmode = optval(args, "-tlsmode");
        if (tlsmode.equalsIgnoreCase("none")) {
          server.getTLSSettings().setTLSMode(TLSSettings.smNoTLS);
        } else if (tlsmode.equalsIgnoreCase("implicit")) {
          server.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);
        }
      }

      if (optext(args, "-usehttp")) {
        server.setUseHTTP(true);
      }

      server.start();

      System.out.println("KMIP server started on port " + server.getPort() + ". Press enter to stop server and exit.");
      input();

      server.stop();

      System.out.println("Server stopped.\n");
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



