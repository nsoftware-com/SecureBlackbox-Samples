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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import secureblackbox.*;

public class smtpclient extends JFrame {

    private JPanel panelServer;
    private JTextField textHost;
    private JSpinner spinnerPort;
    private JRadioButton radioNoTLS;
    private JRadioButton radioExplicitTLS;
    private JRadioButton radioImplicitTLS;
    private JTextField textLogin;
    private JPasswordField textPassword;

    private JPanel panelMessage;
    private JTextField textFromName;
    private JTextField textFromAddress;
    private JTextField textToName;
    private JTextField textToAddress;
    private JTextField textSubject;
    private JRadioButton radioPriorityLowest;
    private JRadioButton radioPriorityLow;
    private JRadioButton radioPriorityNormal;
    private JRadioButton radioPriorityHigh;
    private JRadioButton radioPriorityHighest;
    private JTextArea textMessage;

    private JButton buttonSend;

    private smtpclient() {
        super("SMTP Client");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(null);
        setLocationByPlatform(true);
        setResizable(false);
        setSize(600, 717);
        setVisible(true);

        initializeControls();

        repaint();
        // this is a workaround for repainting the spinner;
        // otherwise, it is shown as a gray rectangle without
        // any text and arrows
        spinnerPort.setVisible(false);
        spinnerPort.setVisible(true);
    }

    private void buttonSendClick() {
        Mailwriter writer = new Mailwriter();
        writer.getFrom().add(new MailAddress(textFromName.getText(), textFromAddress.getText()));
        writer.getSendTo().add(new MailAddress(textToName.getText(), textToAddress.getText()));

        MailMessage message = writer.getMessage();
        Smtpclient client = new Smtpclient();

        try {
            message.setSubject(textSubject.getText());

            if (radioPriorityLowest.isSelected())
                message.setPriority(MailMessage.mpLowest);
            else if (radioPriorityLow.isSelected())
                message.setPriority(MailMessage.mpLow);
            else if (radioPriorityHigh.isSelected())
                message.setPriority(MailMessage.mpHigh);
            else if (radioPriorityHighest.isSelected())
                message.setPriority(MailMessage.mpHighest);

            message.setPlainText(textMessage.getText());
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null,
                    "Failed to compose a message.\n" + err.getMessage(), this.getTitle(), JOptionPane.ERROR_MESSAGE);
        }

        try {
            client.setMessage(message);

            if (radioExplicitTLS.isSelected())
                client.getTLSSettings().setTLSMode(TLSSettings.smExplicitTLS);
            else if (radioImplicitTLS.isSelected())
                client.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);

            client.setUsername(textLogin.getText());
            client.setPassword(new String(textPassword.getPassword()));
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null,
                    "Failed to configure the SMTP client.\n" + err.getMessage(), this.getTitle(), JOptionPane.ERROR_MESSAGE);
        }

        try {
            client.connect(textHost.getText(), (int)spinnerPort.getValue());
            try {
                client.sendMessage();
            }
            finally {
                client.disconnect();
            }

            JOptionPane.showMessageDialog(null, "The message has been sent successfully.",
                    this.getTitle(), JOptionPane.INFORMATION_MESSAGE);
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null,
                    "Failed to send the message.\n" + err.getMessage(), this.getTitle(), JOptionPane.ERROR_MESSAGE);
        }
    }

    private void radioButtonChange(Object source) {
        int port = (int)spinnerPort.getValue();
        if (radioNoTLS.isSelected() || radioExplicitTLS.isSelected()) {
            if (port == 465)
                spinnerPort.setValue(25);
        }
        else
        if (radioImplicitTLS.isSelected()) {
            if (port == 25)
                spinnerPort.setValue(465);
        }
    }

    private void initializeControls() {
        Insets areaFrame = getClientArea(this);

        JLabel label = new JLabel("The sample shows how to prepare and send a mail message quickly using the SMTPClient component.");
        label.setLocation(areaFrame.left + GAP, GAP);
        label.setSize(areaFrame.right - areaFrame.left - DGAP, 18);
        label.setForeground(SystemColor.textHighlight);
        this.add(label);

        panelServer = new JPanel(null);
        panelServer.setBorder(BorderFactory.createTitledBorder(" Mail Server (SMTP) "));
        panelServer.setLocation(GAP, getBottom(label) + GAP);
        panelServer.setSize(areaFrame.right - areaFrame.left - DGAP, 150);
        add(panelServer);
        Insets areaPanel = getClientArea(panelServer);

        label = new JLabel("Host:");
        label.setLocation(areaPanel.left + GAP, areaPanel.top + GAP + 2);
        label.setSize(30, 18);
        panelServer.add(label);

        textHost = new JTextField();
        textHost.setLocation(getRight(label) + GAP, areaPanel.top + GAP);
        textHost.setText("mail.example.com");
        panelServer.add(textHost);

        spinnerPort = new JSpinner(new SpinnerNumberModel(25, 1, 65535, 1));
        spinnerPort.setLocation(areaPanel.right - GAP - 70, textHost.getY());
        spinnerPort.setSize(70, 25);
        panelServer.add(spinnerPort);

        label = new JLabel("Port:");
        label.setSize(30, 18);
        label.setLocation(spinnerPort.getX() - label.getWidth() - GAP, areaPanel.top + GAP + 2);
        panelServer.add(label);

        textHost.setSize(label.getX() - textHost.getX() - DGAP, 25);

        label = new JLabel("Secure connection:");
        label.setSize(110, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(textHost) + GAP);
        panelServer.add(label);

        radioNoTLS = new JRadioButton("Don't use TLS", true);
        radioNoTLS.setLocation(getRight(label) + GAP, getBottom(textHost) + GAP);
        radioNoTLS.setSize(110, 20);
        radioNoTLS.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                radioButtonChange(e.getSource());
            }
        });
        panelServer.add(radioNoTLS);

        radioExplicitTLS = new JRadioButton("Explicit TLS");
        radioExplicitTLS.setLocation(getRight(radioNoTLS) + GAP, radioNoTLS.getY());
        radioExplicitTLS.setSize(100, 20);
        radioExplicitTLS.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                radioButtonChange(e.getSource());
            }
        });
        panelServer.add(radioExplicitTLS);

        radioImplicitTLS = new JRadioButton("Implicit TLS");
        radioImplicitTLS.setLocation(getRight(radioExplicitTLS) + GAP, radioNoTLS.getY());
        radioImplicitTLS.setSize(100, 20);
        radioImplicitTLS.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                radioButtonChange(e.getSource());
            }
        });
        panelServer.add(radioImplicitTLS);

        ButtonGroup group = new ButtonGroup();
        group.add(radioNoTLS);
        group.add(radioExplicitTLS);
        group.add(radioImplicitTLS);

        label = new JLabel("Login (if needed):");
        label.setSize(97, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(radioNoTLS) + GAP + 2);
        panelServer.add(label);

        textLogin = new JTextField("johndow");
        textLogin.setLocation(getRight(label) + GAP, getBottom(radioNoTLS) + GAP);
        textLogin.setSize(getRight(spinnerPort) - textLogin.getX(), 25);
        panelServer.add(textLogin);

        label = new JLabel("Password:", SwingConstants.RIGHT);
        label.setSize(97, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(textLogin) + GAP + 2);
        panelServer.add(label);

        textPassword = new JPasswordField("c00l*p4ssw0rd");
        textPassword.setLocation(getRight(label) + GAP, getBottom(textLogin) + GAP);
        textPassword.setSize(textLogin.getWidth(), 25);
        panelServer.add(textPassword);

        buttonSend = new JButton("Send Message");
        buttonSend.setSize(panelServer.getWidth(), 25);
        buttonSend.setLocation(GAP, areaFrame.bottom - areaFrame.top - buttonSend.getHeight() - DGAP);
        buttonSend.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                buttonSendClick();
            }
        });
        add(buttonSend);

        panelMessage = new JPanel(null);
        panelMessage.setBorder(BorderFactory.createTitledBorder(" Mail Message "));
        panelMessage.setLocation(GAP, getBottom(panelServer) + GAP);
        panelMessage.setSize(panelServer.getWidth(),
                buttonSend.getY() - panelMessage.getY() - GAP);
        add(panelMessage);
        areaPanel = getClientArea(panelMessage);

        label = new JLabel("From:", SwingConstants.RIGHT);
        label.setLocation(areaPanel.left + GAP, areaPanel.top + GAP + 2);
        label.setSize(50, 18);
        panelMessage.add(label);

        textFromName = new JTextField("John Dow");
        textFromName.setLocation(getRight(label) + GAP, areaPanel.top + GAP);
        textFromName.setSize((areaPanel.right - getRight(label) - GAP * 3) / 2, 25);
        panelMessage.add(textFromName);

        textFromAddress = new JTextField("john.dow@example.com");
        textFromAddress.setLocation(getRight(textFromName) + GAP, textFromName.getY());
        textFromAddress.setSize(textFromName.getWidth(), 25);
        panelMessage.add(textFromAddress);

        label = new JLabel("(name)", SwingConstants.CENTER);
        label.setLocation(textFromName.getX(), getBottom(textFromName) - 2);
        label.setSize(textFromName.getWidth(), 18);
        panelMessage.add(label);

        label = new JLabel("(address)", SwingConstants.CENTER);
        label.setLocation(textFromAddress.getX(), getBottom(textFromAddress) - 2);
        label.setSize(textFromAddress.getWidth(), 18);
        panelMessage.add(label);

        textToName = new JTextField("Jane Dow");
        textToName.setLocation(textFromName.getX(), getBottom(label));
        textToName.setSize(textFromName.getWidth(), 25);
        panelMessage.add(textToName);

        textToAddress = new JTextField("jane.dow@example.com");
        textToAddress.setLocation(getRight(textToName) + GAP, textToName.getY());
        textToAddress.setSize(textFromName.getWidth(), 25);
        panelMessage.add(textToAddress);

        label = new JLabel("To:", SwingConstants.RIGHT);
        label.setLocation(areaPanel.left + GAP, textToName.getY() + 2);
        label.setSize(50, 18);
        panelMessage.add(label);

        label = new JLabel("Subject:", SwingConstants.RIGHT);
        label.setLocation(areaPanel.left + GAP, getBottom(textToName) + GAP + 2);
        label.setSize(50, 18);
        panelMessage.add(label);

        textSubject = new JTextField("Good news");
        textSubject.setLocation(textToName.getX(), getBottom(textToName) + GAP);
        textSubject.setSize(getRight(textToAddress) - textSubject.getX(), 25);
        panelMessage.add(textSubject);

        label = new JLabel("Priority:", SwingConstants.RIGHT);
        label.setLocation(areaPanel.left + GAP, getBottom(textSubject) + GAP);
        label.setSize(50, 18);
        panelMessage.add(label);

        radioPriorityLowest = new JRadioButton("Lowest");
        radioPriorityLowest.setLocation(getRight(label) + GAP, getBottom(textSubject) + GAP);
        radioPriorityLowest.setSize(70, 20);
        panelMessage.add(radioPriorityLowest);

        radioPriorityLow = new JRadioButton("Low");
        radioPriorityLow.setLocation(getRight(radioPriorityLowest) + GAP, radioPriorityLowest.getY());
        radioPriorityLow.setSize(50, 20);
        panelMessage.add(radioPriorityLow);

        radioPriorityNormal = new JRadioButton("Normal", true);
        radioPriorityNormal.setLocation(getRight(radioPriorityLow) + GAP, radioPriorityLow.getY());
        radioPriorityNormal.setSize(70, 20);
        panelMessage.add(radioPriorityNormal);

        radioPriorityHigh = new JRadioButton("High");
        radioPriorityHigh.setLocation(getRight(radioPriorityNormal) + GAP, radioPriorityNormal.getY());
        radioPriorityHigh.setSize(50, 20);
        panelMessage.add(radioPriorityHigh);

        radioPriorityHighest = new JRadioButton("Highest");
        radioPriorityHighest.setLocation(getRight(radioPriorityHigh) + GAP, radioPriorityHigh.getY());
        radioPriorityHighest.setSize(70, 20);
        panelMessage.add(radioPriorityHighest);

        group = new ButtonGroup();
        group.add(radioPriorityLowest);
        group.add(radioPriorityLow);
        group.add(radioPriorityNormal);
        group.add(radioPriorityHigh);
        group.add(radioPriorityHighest);

        label = new JLabel("Text (plain):");
        label.setLocation(areaPanel.left + GAP, getBottom(radioPriorityLowest) + GAP);
        label.setSize(70, 18);
        panelMessage.add(label);

        JScrollPane scroller = new JScrollPane();
        scroller.setLocation(areaPanel.left + GAP, getBottom(label) + GAP);
        scroller.setSize(areaPanel.right - areaPanel.left - DGAP,
                areaPanel.bottom - scroller.getY() - GAP);
        textMessage = new JTextArea("... message text goes here ...");
        textMessage.setLineWrap(true);
        textMessage.setWrapStyleWord(true);
        scroller.setViewportView(textMessage);
        panelMessage.add(scroller);
    }

    public static void main(String[] args) {
        new smtpclient();
    }

    private Insets getClientArea(JFrame f) {
        Rectangle b = f.getBounds();
        Insets i = f.getInsets();
        return new Insets(i.top, i.left,b.height - i.bottom,b.width - i.right);
    }

    private Insets getClientArea(JComponent c) {
        Rectangle b = c.getBounds();
        Insets i = c.getInsets();
        return new Insets(i.top, i.left,b.height - i.bottom,b.width - i.right);
    }

    private int getBottom(JComponent c) {
        return c.getY() + c.getHeight();
    }

    private int getRight(JComponent c) {
        return c.getX() + c.getWidth();
    }

    private static final int GAP = 6;
    private static final int DGAP = GAP * 2;

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



