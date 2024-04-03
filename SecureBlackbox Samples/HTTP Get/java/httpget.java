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
import java.awt.*;
import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.TooManyListenersException;

import secureblackbox.*;


public class httpget extends JDialog {

  private static final long serialVersionUID = 1L;
  private JTextField edHost;
  private JTextField edPath;
  private JTextField edOutput;
  private JComboBox<String> cmbProtocol;
  private Httpclient client;
  private JSpinner edPort;
  private JTextArea mmLog;

  /**
   * Launch the application.
   */
  public static void main(String[] args) {
    EventQueue.invokeLater(new Runnable() {
      public void run() {
        try {
          httpget dialog = new httpget();
          dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
          dialog.setVisible(true);
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    });
  }

  /**
   * Create the dialog.
   */
  public httpget() {
    setTitle("HTTP Get");
    setBounds(100, 100, 460, 400);
    getContentPane().setLayout(null);

    JLabel lblCaption = new JLabel("This sample illustrates the ways of making GET requests with HTTPClient.");
    lblCaption.setBounds(10, 5, 450, 14);
    lblCaption.setForeground(new Color(49, 106, 197));
    getContentPane().add(lblCaption);

    JPanel Settingspanel = new JPanel();
    Settingspanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Http options  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
    Settingspanel.setBounds(5, 30, 435, 150);
    getContentPane().add(Settingspanel);
    Settingspanel.setLayout(null);

    JLabel lblProtocol = new JLabel("Protocol");
    lblProtocol.setBounds(10, 28, 61, 14);
    Settingspanel.add(lblProtocol);

    cmbProtocol = new JComboBox<String>();
    cmbProtocol.setModel(new DefaultComboBoxModel<String>(new String[] {"HTTP", "HTTPS"}));
    cmbProtocol.setBounds(70, 25, 77, 22);
    Settingspanel.add(cmbProtocol);

    JLabel lblHost = new JLabel("Host");
    lblHost.setBounds(10, 58, 61, 14);
    Settingspanel.add(lblHost);

    edHost = new JTextField();
    edHost.setText("www.secureblackbox.com");
    edHost.setBounds(70, 55, 159, 20);
    Settingspanel.add(edHost);
    edHost.setColumns(10);

    JLabel lblPort = new JLabel("Port");
    lblPort.setBounds(270, 58, 37, 14);
    Settingspanel.add(lblPort);

    edPort = new JSpinner();
    edPort.setModel(new SpinnerNumberModel(new Integer(80), null, null, new Integer(1)));
    edPort.setBounds(305, 55, 51, 20);
    Settingspanel.add(edPort);

    JLabel lblPath = new JLabel("Path");
    lblPath.setBounds(10, 88, 61, 14);
    Settingspanel.add(lblPath);

    edPath = new JTextField();
    edPath.setText("/");
    edPath.setBounds(70, 85, 270, 20);
    Settingspanel.add(edPath);
    edPath.setColumns(10);

    JLabel lblSaveTo = new JLabel("Save to");
    lblSaveTo.setBounds(10, 118, 61, 14);
    Settingspanel.add(lblSaveTo);

    edOutput = new JTextField();
    edOutput.setBounds(70, 115, 270, 20);
    Settingspanel.add(edOutput);
    edOutput.setColumns(10);

    JButton btnBrowse = new JButton("Browse");
    btnBrowse.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        edOutput.setText(getSaveFileName());
      }
    });
    btnBrowse.setBounds(345, 113, 80, 25);
    Settingspanel.add(btnBrowse);

    JButton btnRetrieve = new JButton("Get");
    btnRetrieve.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        retrieveClick();
      }
    });
    btnRetrieve.setBounds(358, 185, 80, 25);
    getContentPane().add(btnRetrieve);

    JScrollPane scrollPane = new JScrollPane();
    scrollPane.setBounds(5, 220, 435, 135);
    getContentPane().add(scrollPane);

    mmLog = new JTextArea();
    scrollPane.setViewportView(mmLog);

    init();
  }

  private void init() {
    client = new Httpclient();
    try
    {
      client.getTLSSettings().setAutoValidateCertificates(false);
    }
    catch (SecureBlackboxException ex)
    {

    }
    initEvents();
  }

  private void initEvents() {
    try {
      client.addHttpclientEventListener(new HttpclientEventListener() {
        
        @Override
        public void cookie(HttpclientCookieEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void documentBegin(HttpclientDocumentBeginEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void documentEnd(HttpclientDocumentEndEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void dynamicDataNeeded(HttpclientDynamicDataNeededEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void error(HttpclientErrorEvent e) {
          mmLog.append(e.errorCode + ": " + e.description + "\n");
        }
        
        @Override
        public void externalSign(HttpclientExternalSignEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void headersPrepared(HttpclientHeadersPreparedEvent e) {
          mmLog.append("\nSending headers: \n");
          StringNameValuePair element;
          for (int i = 0; i < client.getRequestHeaders().size(); i++) {
            element = client.getRequestHeaders().item(i);
            mmLog.append(element.getName() + ": " + element.getValue() + "\n");
          }
        }
        
        @Override
        public void headersReceived(HttpclientHeadersReceivedEvent e) {
          mmLog.append("\nReceived headers: \n");
          StringNameValuePair element;
          for (int i = 0; i < client.getResponseHeaders().size(); i++) {
            element = client.getResponseHeaders().item(i);
            mmLog.append(element.getName() + ": " + element.getValue() + "\n");
          }
        }
        
        @Override
        public void notification(HttpclientNotificationEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void progress(HttpclientProgressEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void redirection(HttpclientRedirectionEvent e) {
          mmLog.append("\nRedirected to " + e.newURL + "\n");
          e.newURL = e.newURL; // can be changed here
          e.allowRedirection = true;
        }

        @Override
        public void TLSCertNeeded(HttpclientTLSCertNeededEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void TLSCertValidate(HttpclientTLSCertValidateEvent e) {
          e.accept = true;
        }

        @Override
        public void TLSEstablished(HttpclientTLSEstablishedEvent e) {
          // TODO Auto-generated method stub
        }

        @Override
        public void TLSHandshake(HttpclientTLSHandshakeEvent e) {
          // TODO Auto-generated method stub
        }

        @Override
        public void TLSPSK(HttpclientTLSPSKEvent e) {
          // TODO Auto-generated method stub
        }

        @Override
        public void TLSShutdown(HttpclientTLSShutdownEvent e) {
          // TODO Auto-generated method stub
        }
        
        @Override
        public void supercoreIntercept(HttpclientSupercoreInterceptEvent e) {
          // TODO Auto-generated method stub
        }
      });
    }
    catch (TooManyListenersException ex)
    {

    }
  }

  private void showErrorMessage(String msg) {
    JOptionPane.showMessageDialog(null, msg, getTitle(), JOptionPane.ERROR_MESSAGE);
  }

  String getSaveFileName(){
    JFileChooser fc = new JFileChooser();
    int returnVal = fc.showSaveDialog(this);
    if (returnVal == JFileChooser.APPROVE_OPTION)
      return fc.getSelectedFile().getPath();

    return "";
  }

  protected void retrieveClick() {
    String URL = cmbProtocol.getSelectedItem().toString().toLowerCase() + "://" + edHost.getText() + ":" + edPort.getValue().toString() + edPath.getText();

    try
    {
      if (edOutput.getText().length() > 0)
      {
        client.getFile(URL, edOutput.getText());
      }
      else
      {
        client.get(URL);
      }

      mmLog.append(client.getOutputString());
    }
    catch (Exception E)
    {
      showErrorMessage("Exception happened during HTTP download: " + E.getMessage());
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



