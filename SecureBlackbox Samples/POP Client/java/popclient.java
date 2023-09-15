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
import javax.swing.table.DefaultTableModel;

import secureblackbox.*;

public class popclient extends JFrame {

    private JPanel panelServer;
    private JTextField textHost;
    private JSpinner spinnerPort;
    private JRadioButton radioNoTLS;
    private JRadioButton radioExplicitTLS;
    private JRadioButton radioImplicitTLS;
    private JTextField textLogin;
    private JPasswordField textPassword;
    private JButton buttonConnect;
    private JButton buttonDisconnect;

    private JPanel panelList;
    private JScrollPane scrollMessages;
    private DefaultTableModel modelMessages;
    private JTable tableMessages;
    private JButton buttonDelete;
    private JButton buttonReceive;

    private JPanel panelMessage;
    private JTextField textFrom;
    private JTextField textTo;
    private JTextField textDate;
    private JTextField textPriority;
    private JTextField textSubject;
    private JTextArea textPlain;
    private JTextArea textHtml;
    private JLabel labelAttachCount;

    private Pop3client client;

    private popclient() {
        super("POP3 Client");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(null);
        setLocationByPlatform(true);
        setResizable(false);
        setSize(830, 628);
        setVisible(true);

        initializeControls();
        repaint();
        // this is a workaround for repainting the spinner;
        // otherwise, it is shown as a gray rectangle without
        // any text and arrows
        spinnerPort.setVisible(false);
        spinnerPort.setVisible(true);
    }

    private void buttonConnectClick() {
        if (client != null) {
            JOptionPane.showMessageDialog(null, "Already connected. Please disconnect first.",
                    this.getTitle(), JOptionPane.WARNING_MESSAGE);
            return;
        }

        try {
            client = new Pop3client();
            client.config("RequestUIDs=true");

            if (radioExplicitTLS.isSelected())
                client.getTLSSettings().setTLSMode(TLSSettings.smExplicitTLS);
            else if (radioImplicitTLS.isSelected())
                client.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);

            client.setUsername(textLogin.getText());
            client.setPassword(new String(textPassword.getPassword()));
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null, "Failed to configure the POP3 client.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            client = null;
            return;
        }

        try {
            client.connect(textHost.getText(), (int)spinnerPort.getValue());
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null, "Failed to connec to the server.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            client = null;
            return;
        }

        JOptionPane.showMessageDialog(null, "Connected to the server. Listing messages...",
                this.getTitle(), JOptionPane.INFORMATION_MESSAGE);
        try {
            client.listMessages();

            int count = client.getMessages().size();
            if (count == 0)
                JOptionPane.showMessageDialog(null, "No messages on the server.",
                        this.getTitle(), JOptionPane.INFORMATION_MESSAGE);
            else {
                loadMessages();
                JOptionPane.showMessageDialog(null, String.format("There is(are) %d message(s) on the server.", count),
                        this.getTitle(), JOptionPane.INFORMATION_MESSAGE);
            }
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null, "Failed to list messages on the server.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            try {
                client.disconnect();
            }
            catch (SecureBlackboxException err2) {
                //
            }
            client = null;
        }
    }

    private void buttonDisconnectClick() {
        if (client == null) {
            JOptionPane.showMessageDialog(null, "Not connected.", this.getTitle(), JOptionPane.INFORMATION_MESSAGE);
            return;
        }

        clearMessages();
        try {
            client.disconnect();
        }
        catch (SecureBlackboxException err) {
            //
        }
        client = null;
        JOptionPane.showMessageDialog(null, "Disconnected.", this.getTitle(), JOptionPane.INFORMATION_MESSAGE);
    }

    private void buttonDeleteClick() {
        if (client == null) {
            JOptionPane.showMessageDialog(null, "Not connected. Please connect to a POP3 server first.",
                    this.getTitle(), JOptionPane.WARNING_MESSAGE);
            return;
        }

        if (tableMessages.getSelectedRowCount() == 0) {
            JOptionPane.showMessageDialog(null, "Please select a message to delete.",
                    this.getTitle(), JOptionPane.WARNING_MESSAGE);
            return;
        }

        int index = tableMessages.getSelectedRow();
        if (JOptionPane.showConfirmDialog(null,
                String.format("Are you sure you want to delete the message \"%s\"?", modelMessages.getValueAt(index, 0)),
                this.getTitle(), JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION)
            return;

        try {
            client.deleteMessage(index);
            loadMessages();
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null, "Failed to delete the message.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
        }
    }

    private void buttonReceiveClick() {
        if (client == null) {
            JOptionPane.showMessageDialog(null, "Not connected. Please connect to a POP3 server first.",
                    this.getTitle(), JOptionPane.WARNING_MESSAGE);
            return;
        }

        if (tableMessages.getSelectedRowCount() == 0) {
            JOptionPane.showMessageDialog(null, "Please select a message to receive.",
                    this.getTitle(), JOptionPane.WARNING_MESSAGE);
            return;
        }

        try {
            clearMessage();
            client.receiveMessage(tableMessages.getSelectedRow());
            loadMessage();
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(null, "Failed to receive the message.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
        }
    }

    private void radioButtonChange(Object source) {
        int port = (int)spinnerPort.getValue();
        if (radioNoTLS.isSelected() || radioExplicitTLS.isSelected()) {
            if (port == 995)
                spinnerPort.setValue(110);
        }
        else
        if (radioImplicitTLS.isSelected()) {
            if (port == 110)
                spinnerPort.setValue(995);
        }
    }

    private void clearMessage() {
        textFrom.setText("");
        textTo.setText("");
        textDate.setText("");
        textPriority.setText("");
        textSubject.setText("");
        textPlain.setText("");
        textHtml.setText("");
        labelAttachCount.setText("[none]");
    }

    private void clearMessages() {
        modelMessages.setRowCount(0);
    }

    private void loadMessage() {
        MailMessage message = client.getMessage();
        textFrom.setText(message.getFrom());
        textTo.setText(message.getSendTo());
        textDate.setText(message.getDate());
        textSubject.setText(message.getSubject());
        switch (message.getPriority()) {
            case MailMessage.mpLowest:
                textPriority.setText("[lowest]");
                break;
            case MailMessage.mpLow:
                textPriority.setText("[low]");
                break;
            case MailMessage.mpNormal:
                textPriority.setText("[normal]");
                break;
            case MailMessage.mpHigh:
                textPriority.setText("[HIGH]");
                break;
            case MailMessage.mpHighest:
                textPriority.setText("[HIGHEST]");
                break;
        }

        textPlain.setText(message.getPlainText());
        textHtml.setText(message.getHtmlText());

        int count = message.getAttachmentCount();
        if (count == 0)
            labelAttachCount.setText("[none]");
        else
            labelAttachCount.setText(Integer.toString(count));
    }

    private void loadMessages() {
        if (client == null)
            return;

        int count = client.getMessages().size();
        modelMessages.setRowCount(count);
        for (int i = 0; i < count; i++) {
            POP3MessageInfo info = client.getMessages().item(i);
            modelMessages.setValueAt(info.getUID(), i, 0);
            modelMessages.setValueAt(info.getSize(), i, 1);
        }
    }

    private void initializeControls() {
        Insets areaFrame = getClientArea(this);

        // Sample description label

        JLabel label = new JLabel("This sample shows how to deal with POP3 servers. It can list messages on a server, receive them from the server and delete them.");
        label.setLocation(areaFrame.left + GAP, GAP);
        label.setSize(areaFrame.right - areaFrame.left - DGAP, 18);
        label.setForeground(SystemColor.textHighlight);
        this.add(label);

        // Server groupbox

        panelServer = new JPanel(null);
        panelServer.setBorder(BorderFactory.createTitledBorder(" Mail Server (POP3) "));
        panelServer.setLocation(GAP, getBottom(label) + GAP);
        panelServer.setSize(430, 180);
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

        spinnerPort = new JSpinner(new SpinnerNumberModel(110, 1, 65535, 1));
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
        radioNoTLS.setSize(103, 20);
        radioNoTLS.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                radioButtonChange(e.getSource());
            }
        });
        panelServer.add(radioNoTLS);

        radioExplicitTLS = new JRadioButton("Explicit TLS");
        radioExplicitTLS.setLocation(getRight(radioNoTLS) + GAP, radioNoTLS.getY());
        radioExplicitTLS.setSize(91, 20);
        radioExplicitTLS.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                radioButtonChange(e.getSource());
            }
        });
        panelServer.add(radioExplicitTLS);

        radioImplicitTLS = new JRadioButton("Implicit TLS");
        radioImplicitTLS.setLocation(getRight(radioExplicitTLS) + GAP, radioNoTLS.getY());
        radioImplicitTLS.setSize(91, 20);
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

        label = new JLabel("Login:", SwingConstants.RIGHT);
        label.setSize(61, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(radioNoTLS) + GAP + 2);
        panelServer.add(label);

        textLogin = new JTextField("johndow");
        textLogin.setLocation(getRight(label) + GAP, getBottom(radioNoTLS) + GAP);
        textLogin.setSize(getRight(spinnerPort) - textLogin.getX(), 25);
        panelServer.add(textLogin);

        label = new JLabel("Password:", SwingConstants.RIGHT);
        label.setSize(61, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(textLogin) + GAP + 2);
        panelServer.add(label);

        textPassword = new JPasswordField("c00l*p4ssw0rd");
        textPassword.setLocation(getRight(label) + GAP, getBottom(textLogin) + GAP);
        textPassword.setSize(textLogin.getWidth(), 25);
        panelServer.add(textPassword);

        buttonConnect = new JButton("Connect");
        buttonConnect.setLocation(areaPanel.left + GAP, getBottom(textPassword) + GAP);
        buttonConnect.setSize((areaPanel.right - areaPanel.left - TGAP) / 2, 25);
        buttonConnect.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonConnectClick();
            }
        });
        panelServer.add(buttonConnect);

        buttonDisconnect = new JButton("Disconnect");
        buttonDisconnect.setLocation(getRight(buttonConnect) + GAP, buttonConnect.getY());
        buttonDisconnect.setSize(buttonConnect.getWidth(), buttonConnect.getHeight());
        buttonDisconnect.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonDisconnectClick();
            }
        });
        panelServer.add(buttonDisconnect);

        // Message List groupbox

        panelList = new JPanel(null);
        panelList.setBorder(BorderFactory.createTitledBorder(" Message List "));
        panelList.setLocation(GAP, getBottom(panelServer) + GAP);
        panelList.setSize(panelServer.getWidth(), areaFrame.bottom - areaFrame.top - GAP - panelList.getY());
        add(panelList);
        areaPanel = getClientArea(panelList);

        buttonDelete = new JButton("Delete Message");
        buttonDelete.setSize((areaPanel.right - areaPanel.left - TGAP) / 2, 25);
        buttonDelete.setLocation(areaPanel.left + GAP, areaPanel.bottom - GAP - buttonDelete.getHeight());
        buttonDelete.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonDeleteClick();
            }
        });
        panelList.add(buttonDelete);

        buttonReceive = new JButton("Receive Message ->");
        buttonReceive.setLocation(getRight(buttonDelete) + GAP, buttonDelete.getY());
        buttonReceive.setSize(buttonDelete.getWidth(), buttonDelete.getHeight());
        buttonReceive.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonReceiveClick();
            }
        });
        panelList.add(buttonReceive);

        scrollMessages = new JScrollPane();
        scrollMessages.setLocation(areaPanel.left + GAP, areaPanel.top + GAP);
        scrollMessages.setSize(areaPanel.right - areaPanel.left - DGAP, buttonDelete.getY() - areaPanel.top - DGAP);
        modelMessages = new DefaultTableModel(COLUMN_TITLES, 0);
        tableMessages = new JTable(modelMessages);
        tableMessages.getColumnModel().getColumn(0).setPreferredWidth((int)(scrollMessages.getWidth() * 0.75));
        tableMessages.getColumnModel().getColumn(1).setPreferredWidth((int)(scrollMessages.getWidth() * 0.25));
        scrollMessages.setViewportView(tableMessages);
        panelList.add(scrollMessages);

        // Message groupbox

        panelMessage = new JPanel(null);
        panelMessage.setBorder(BorderFactory.createTitledBorder(" Message "));
        panelMessage.setLocation(getRight(panelServer) + GAP, panelServer.getY());
        panelMessage.setSize(areaFrame.right - areaFrame.left - TGAP - panelServer.getWidth(),
                areaFrame.bottom - areaFrame.top - panelMessage.getY() - GAP);
        add(panelMessage);
        areaPanel = getClientArea(panelMessage);

        label = new JLabel("From:", SwingConstants.RIGHT);
        label.setSize(46, 18);
        label.setLocation(areaPanel.left + GAP, areaPanel.top + GAP + 2);
        panelMessage.add(label);

        textFrom = new JTextField();
        textFrom.setEditable(false);
        textFrom.setLocation(getRight(label) + GAP, areaPanel.top + GAP);
        textFrom.setSize(areaPanel.right - areaPanel.left - label.getWidth() - TGAP, 25);
        panelMessage.add(textFrom);

        label = new JLabel("To:", SwingConstants.RIGHT);
        label.setSize(46, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(textFrom) + GAP + 2);
        panelMessage.add(label);

        textTo = new JTextField();
        textTo.setEditable(false);
        textTo.setLocation(textFrom.getX(), getBottom(textFrom) + GAP);
        textTo.setSize(textFrom.getWidth(), textFrom.getY());
        panelMessage.add(textTo);

        label = new JLabel("Date:", SwingConstants.RIGHT);
        label.setSize(46, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(textTo) + GAP + 2);
        panelMessage.add(label);

        textDate = new JTextField();
        textDate.setEditable(false);
        textDate.setLocation(textTo.getX(), getBottom(textTo) + GAP);

        textPriority = new JTextField();
        textPriority.setEditable(false);
        textPriority.setSize(65, 25);
        textPriority.setLocation(areaPanel.right - textPriority.getWidth() - GAP, getBottom(textTo) + GAP);
        panelMessage.add(textPriority);

        label = new JLabel("Priority:");
        label.setSize(46, 18);
        label.setLocation(textPriority.getX() - label.getWidth() - GAP, textPriority.getY() + 2);
        panelMessage.add(label);

        textDate.setSize(label.getX() - textDate.getX() - DGAP, 25);
        panelMessage.add(textDate);

        label = new JLabel("Subject:");
        label.setSize(46, 18);
        label.setLocation(areaPanel.left + GAP, getBottom(textDate) + GAP + 2);
        panelMessage.add(label);

        textSubject = new JTextField();
        textSubject.setEditable(false);
        textSubject.setLocation(getRight(label) + GAP, getBottom(textDate) + GAP);
        textSubject.setSize(areaPanel.right - textSubject.getX() - GAP, 25);
        panelMessage.add(textSubject);

        label = new JLabel("Attachments:");
        label.setSize(76, 18);
        label.setLocation(areaPanel.left + GAP, areaPanel.bottom - GAP - label.getHeight());
        panelMessage.add(label);

        labelAttachCount = new JLabel("[none]");
        labelAttachCount.setSize(36, 18);
        labelAttachCount.setLocation(getRight(label) + GAP, label.getY());
        panelMessage.add(labelAttachCount);

        label = new JLabel("Text (plain):");
        label.setLocation(areaPanel.left + GAP, getBottom(textSubject) + GAP);
        label.setSize(66, 18);
        panelMessage.add(label);

        JScrollPane scroller = new JScrollPane();
        scroller.setLocation(label.getX(), getBottom(label) + HGAP);
        scroller.setSize(areaPanel.right - areaPanel.left - DGAP,
                (labelAttachCount.getY() - getBottom(label) - TGAP - label.getHeight()) / 2);
        textPlain = new JTextArea();
        textPlain.setEditable(false);
        scroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scroller.setViewportView(textPlain);
        panelMessage.add(scroller);

        label = new JLabel("Text (HTML):");
        label.setLocation(areaPanel.left + GAP, getBottom(scroller) + GAP);
        label.setSize(71, 18);
        panelMessage.add(label);

        scroller = new JScrollPane();
        scroller.setLocation(label.getX(), getBottom(label) + HGAP);
        scroller.setSize(areaPanel.right - areaPanel.left - DGAP,
                labelAttachCount.getY() - getBottom(label) - GAP - HGAP);
        textHtml = new JTextArea();
        textHtml.setEditable(false);
        scroller.setViewportView(textHtml);
        scroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        panelMessage.add(scroller);
    }

    public static void main(String[] args) {
        new popclient();
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
    private static final int HGAP = GAP / 2;
    private static final int DGAP = GAP * 2;
    private static final int TGAP = GAP * 3;

    private static final String[] COLUMN_TITLES = { "UID", "Size" };
    private static final Class[] COLUMN_CLASSES = { String.class, Long.class };
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



