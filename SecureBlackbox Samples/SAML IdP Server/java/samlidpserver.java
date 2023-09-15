/*
 * SecureBlackbox 2022 Java Edition - Sample Project
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
import java.util.Scanner;

public class samlidpserver {

    private static final Samlidpserver samlServer = new Samlidpserver();
    private static final Tlsserver tlsServer = new Tlsserver();
    private static Scanner scanner;
    private static boolean externalServerMode = true;

    public static void main(String[] args) {
        scanner = new Scanner(System.in);
        try {
            Runtime.getRuntime().addShutdownHook(new Thread(new ServerStop()));
            runServer();
        } catch (Exception ex) {
            System.out.println("Error: " + ex.getMessage());
            ex.printStackTrace();
        }
    }

    static void runServer() throws Exception {
        final String baseDir = System.getProperty("user.home") + "/sbbsamltest";
        extractFiles(baseDir);

        samlServer.setPort(9090);
        tlsServer.setPort(9090);
        samlServer.setURL("http://idp.test.org:9090");
        samlServer.setMetadataURL("/idp/metadata");
        samlServer.setSingleSignOnService("/idp/sso");
        samlServer.setSingleLogoutService("/idp/sls");
        samlServer.setArtifactResolutionService("/idp/ars");
        samlServer.setSingleSignOnServiceBindings("+Redirect,+POST");
        samlServer.setSingleLogoutServiceBindings("+Redirect,+POST");
        samlServer.setAuthFormTemplate(LOGIN_PAGE_CODE);

        Certificate cert = new Certificate(baseDir + "/" + CERT_FILE, CERT_PASSWORD);
        samlServer.setSigningCertificate(cert);
        samlServer.setEncryptionCertificate(cert);
        samlServer.setMetaSigningCertificate(cert);

        samlServer.getServerCertificates().add(cert); // in case of TLS usage
        samlServer.getTLSSettings().setTLSMode(secureblackbox.TLSSettings.smNoTLS);

        samlServer.saveMetadata(baseDir + "/idp_metadata.xml");

        System.out.println("SAML IdP server saved its metadata to " + baseDir);
        System.out.println("Please run SAML SP sample and then press <Enter>:");
        scanner.nextLine();

        System.out.println("Reading SP metadata file...");
        samlServer.loadSPMetadata(baseDir + "/sp_metadata.xml");

        samlServer.setOfflineMode(externalServerMode);

        tlsServer.addTlsserverEventListener(new TLSEventListener());

        samlServer.addUserWithEmail("user", "sales@nsoftware.com", "1234");

        System.out.print("Starting SAML IdP server...");
        try {
            samlServer.start();
            System.out.println("DONE!");
            if (externalServerMode) {
                tlsServer.start();
                System.out.println("Press <Enter> to stop server");
                scanner.nextLine();
            }
        } catch (Exception ex) {
            System.out.println("Error: " + ex.getMessage());
            ex.printStackTrace();
        }
    }

    static void extractFiles(String path) throws IOException {
        File dir = new File(path);
        if (!dir.exists()) dir.mkdirs();

        File file = createNewFile(dir, CERT_FILE);
        writeStringToFile(file, CERTIFICATE);
    }

    static File createNewFile(File parent, String name) throws IOException {
        File file = new File(parent, name);

        if (file.exists() && file.isFile())
            file.delete();

        if (file.createNewFile())
            return file;
        else
            throw new IOException("failed to create new file");
    }

    static void writeStringToFile(File file, String s) throws IOException {
        OutputStream os = new FileOutputStream(file);
        try {
            byte[] buf = s.getBytes("UTF-8");
            os.write(buf);
        } finally {
            os.close();
        }
    }

    static class TLSEventListener implements TlsserverEventListener {

        @Override
        public void accept(TlsserverAcceptEvent args) {

        }

        @Override
        public void certificateValidate(TlsserverCertificateValidateEvent args) {

        }

        @Override
        public void connect(TlsserverConnectEvent args) {

        }

        @Override
        public void data(TlsserverDataEvent args) {
            try {
                byte[] response = samlServer.processGenericRequest(args.buffer);
                tlsServer.sendData(args.connectionID, response);
            } catch (Exception ex) {
                System.out.println("Error: " + ex.getMessage());
                ex.printStackTrace();
            }
        }

        @Override
        public void disconnect(TlsserverDisconnectEvent args) {

        }

        @Override
        public void error(TlsserverErrorEvent args) {

        }

        @Override
        public void externalSign(TlsserverExternalSignEvent args) {

        }

        @Override
        public void notification(TlsserverNotificationEvent args) {

        }

        @Override
        public void TLSEstablished(TlsserverTLSEstablishedEvent args) {

        }

        @Override
        public void TLSHandshake(TlsserverTLSHandshakeEvent args) {

        }

        @Override
        public void TLSPSK(TlsserverTLSPSKEvent args) {

        }

        @Override
        public void TLSShutdown(TlsserverTLSShutdownEvent args) {

        }

        @Override
        public void supercoreIntercept(TlsserverSupercoreInterceptEvent args) {

        }
    };

    static class ServerStop implements Runnable {
        @Override
        public void run() {
            try {
                if (externalServerMode) {
                    tlsServer.stop();
                }
                samlServer.stop();
                if (scanner != null) scanner.close();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    // ------------ //

    private static final String CERT_FILE = "idpcert.pem";
    private static final String CERT_PASSWORD = "password";

    private static final String LOGIN_PAGE_CODE = "<html><head><title>SecureBlackbox SAML Login</title></head>" +
            "<body><center><h1>Enter login credentials</h2><form action=\"%URL%\" method=\"post\">" +
            "Login:<br/><input type=\"text\" name=\"login\"/><br/>" +
            "Password:<br/><input type=\"password\" name=\"password\"/><br/>" +
            "<input type=\"submit\" value=\"Login\"/>" +
            "</form></center></body></html>";

    private static final String CERTIFICATE = "-----BEGIN CERTIFICATE-----\n" +
            "MIIEKDCCAxCgAwIBAgIFYA3A3gIwDQYJKoZIhvcNAQELBQAwfjELMAkGA1UEBhMC\n" +
            "VVMxCzAJBgNVBAgTAk5DMRQwEgYDVQQHEwtDaGFwZWwgSGlsbDEiMCAGCSqGSIb3\n" +
            "DQEJARMTc2FsZXNAbnNvZnR3YXJlLmNvbTEoMCYGA1UEAxMfU2VjdXJlQmxhY2ti\n" +
            "b3ggRGVtbyBDZXJ0aWZpY2F0ZTAeFw0xNzA5MDEwMDAwMDBaFw0yMjA5MDEwMDAw\n" +
            "MDBaMH4xCzAJBgNVBAYTAlVTMQswCQYDVQQIEwJOQzEUMBIGA1UEBxMLQ2hhcGVs\n" +
            "IEhpbGwxIjAgBgkqhkiG9w0BCQETE3NhbGVzQG5zb2Z0d2FyZS5jb20xKDAmBgNV\n" +
            "BAMTH1NlY3VyZUJsYWNrYm94IERlbW8gQ2VydGlmaWNhdGUwggEiMA0GCSqGSIb3\n" +
            "DQEBAQUAA4IBDwAwggEKAoIBAQCfAVIcq9oeoIbw56LsPjxWsX3YGkqasBQzgQym\n" +
            "hfcDZeSvFKFis8iqArGLTUUWOaImO5t2UmjIa+4nYP39hb9Z1r0CMy53p83oYbbZ\n" +
            "Z7qMeji0pMAufJHqKCN8t2HNWf2HE8S9UOU7R/UHdrSANJitBKV9lSG9f450yWoG\n" +
            "Nwt35ZCsOp0zqtCgvkEvteGsz01R5DDjOccs3QNu25K/Sf27KPCYabS6A1ehYNY/\n" +
            "G32qoKNJhu3DN4bWje57gBWi9WSfQ3tZUOV5q2ozaNJA78Wl4fXC6RBCaBh0GOwX\n" +
            "eyZoH13LJdn+lqlCSMz024ImG4yFQEENbWvE9Elku8RYTn73AgMBAAGjgawwgakw\n" +
            "HwYDVR0jBBgwFoAUmt/L7GI1SH36Sp8EUBxTwuzSgqMwHQYDVR0OBBYEFJrfy+xi\n" +
            "NUh9+kqfBFAcU8Ls0oKjMA8GA1UdEwEB/wQFMAMBAf8wDwYDVR0PAQH/BAUDAwDu\n" +
            "ADBFBgNVHSUEPjA8BggrBgEFBQcDAQYIKwYBBQUHAwIGCCsGAQUFBwMDBggrBgEF\n" +
            "BQcDBAYIKwYBBQUHAwgGCCsGAQUFBwMJMA0GCSqGSIb3DQEBCwUAA4IBAQBovHBb\n" +
            "MsW/k/5iuLQQqsYSng5X2iy1W+5BRb1hz0MGnHGLqJWy1ty5+bTo6g/zvT65dXIP\n" +
            "IexLkInEenYSy75Lthr6aewUcvwfl1TYXjWlMD5Nm5pM9As71+XsGSdYGXoKohbE\n" +
            "zbT6RDByjwR+yxtatqko7e1Eg6InNJRTRt7al/63FSPEgSCqOX6asDVDNZ83db0d\n" +
            "OcoeaPEiDz3liE3+tYHtKXj5/qwTtYdaqBZxdJfuCKZveEFe1DO3/ayDvIvG9Eme\n" +
            "+rEjntErF+Cw9a8ukesvDuT49cRE9oTs3O7f6LUbhCv5zJN+dTFr75NSdWdp4yvn\n" +
            "3nK3i2udeMnTWK0U\n" +
            "-----END CERTIFICATE-----\n" +
            "-----BEGIN RSA PRIVATE KEY-----\n" +
            "MIIEowIBAAKCAQEAnwFSHKvaHqCG8Oei7D48VrF92BpKmrAUM4EMpoX3A2XkrxSh\n" +
            "YrPIqgKxi01FFjmiJjubdlJoyGvuJ2D9/YW/Wda9AjMud6fN6GG22We6jHo4tKTA\n" +
            "LnyR6igjfLdhzVn9hxPEvVDlO0f1B3a0gDSYrQSlfZUhvX+OdMlqBjcLd+WQrDqd\n" +
            "M6rQoL5BL7XhrM9NUeQw4znHLN0DbtuSv0n9uyjwmGm0ugNXoWDWPxt9qqCjSYbt\n" +
            "wzeG1o3ue4AVovVkn0N7WVDleatqM2jSQO/FpeH1wukQQmgYdBjsF3smaB9dyyXZ\n" +
            "/papQkjM9NuCJhuMhUBBDW1rxPRJZLvEWE5+9wIEAAEAAQKCAQAVDpC4OU32dl3I\n" +
            "c3SRAqDs/i5jOwAcQis6suFhACa5LXo+cWX580MkuJyYjRAUJGfD1mr9HWbA/AIa\n" +
            "9u5L77VqiVSYgPyrXe6X0rDrU6BR0DHIQnUs8tREBTLF5mcuUbeQdt68D4V790iM\n" +
            "x3yZ6d06NH5SBjjkporQDoha8Fk1dFzm8JLmsqiJ7pMkdhmYk1Vw4Rp5qKnjt2Q8\n" +
            "PW94R7TPsvYBw6sZtjRobWF12ItRj3DsrNMry6Ya2jMa1lbz6UAQt4H5poVSbNx2\n" +
            "j2fPXFr6qA4yBSDsVusAry86oiqrmKlBSJf2DMsf/OXRYb1G6iEjGSZmu4EXV5Lh\n" +
            "24PM0h0hAoGBAMtuf1RQ409/LnN7g/keM0AdAEPELBCVmPLVxMk9opCe4j+L3tPs\n" +
            "xk5k478Jijge+Jjz9xT73hx5EFy5u+suT0Nb5rrwCbC13gBuiJVdlXbmDHwq+sYJ\n" +
            "PTG2s27pdDgJ20I2d50ZT6XqXfyZa/SqBcdhICVxkyWPE0lqRDmXpeTHAoGBAMgX\n" +
            "5y4f4SjNHCVHI6jxVh1sYvyVoBaPrU9J9avP2BAvP8TCwHosFxIRC+p8Y23kxbqh\n" +
            "nHnPrC+BQsEvqDWZEjSsPCM4RRVLL5Ssi5JvTwepl68oKLlhut0To34/PckfBAu2\n" +
            "myV8ZSc/Qew5HWo/IiCpeHbu8m/kvnPyIyIW0gRRAoGAbdWi+asSj4FjWwZ9Iquj\n" +
            "kCIYWn44LFwY0EOKk/yrINon3ZdvH4zfJMRkEmjcL6DJKAvQg4vZSBY1N1swkrHW\n" +
            "KdAcqyGemcIXsrP7GmoEoMVkSEYMT4/9cjHqfx+cmAT6VK2Dt1ZLIpF/XTqYhuil\n" +
            "ZhiSeqTvhX/SMfkmDNJLx1ECgYBVZ8Z4LIlJhgq+apfw81nDY1D46NCU8KyTzEi9\n" +
            "FhDuD4zTaiEHxKvwPTBt3Lv/2wb+ux5z681d1Xltxe6xOe3IZ7+fdNQf26Cqf99v\n" +
            "GG2OIrgoviZrwiglVs2RBGNV5hQu/7lHDcqLLvfvYQ2KHPpvIIbXlPQMCIG2MaMH\n" +
            "WOA5kQKBgC5EegmJd0f60bFaGRXGzT6St0sW3d3YJkOg6PwmE7HVXHPPCbwEICmb\n" +
            "uTcSsWNaDnf23UQx0jmNnoKeMsE+ApKvitl5/aaFz7t2FAKDr8NmseKH2S99KJLQ\n" +
            "kw+BznhWFwuZXYdNyKRm7V8Esb92cPsEcAIgktzkEqRS4qLCn+cl\n" +
            "-----END RSA PRIVATE KEY-----\n";
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

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
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



