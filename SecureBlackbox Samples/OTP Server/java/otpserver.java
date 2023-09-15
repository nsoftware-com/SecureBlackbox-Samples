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

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;


public class otpserver extends JDialog implements ActionListener {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField eKeySecret;
	private JSpinner ePassLen;
    private JComboBox<String> cAlgorithm;
    private JLabel lInterval;
    private JSpinner eInterval;
    private JLabel lHashAlgorithm;
    private JComboBox<String> cHashAlgorithm;
    private JCheckBox cUseBaseTime;
    private JSpinner eBaseTime;
    private JSpinner eDelta;
    private JTextField ePassword;
    JButton bValidate;

    Otpserver server;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			otpserver dialog = new otpserver();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			showErrorMessage(e.getMessage());
		}
	}

	/**
	 * Create the dialog.
	 */
	public otpserver() {
        server = new Otpserver();

		setTitle("OTP Server Demo");
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        setBounds((screenSize.width - 400) / 2, (screenSize.height - 380) / 2, 400, 440);

        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        contentPanel.setLayout(null);

        JLabel lblCaption = new JLabel("This sample acts as a basic One-Time-Password protocol server.");
        lblCaption.setBounds(10, 5, 390, 14);
        lblCaption.setForeground(new Color(49, 106, 197));
        contentPanel.add(lblCaption);

        JPanel panelOptions = new JPanel();
        panelOptions.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Security parameters  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
        panelOptions.setBounds(5, 30, 375, 260);
        contentPanel.add(panelOptions);
        panelOptions.setLayout(null);

        JLabel lblNewLabel_1 = new JLabel("Key secret:");
        lblNewLabel_1.setBounds(10, 28, 80, 15);
        panelOptions.add(lblNewLabel_1);
        {
            eKeySecret = new JTextField();
            eKeySecret.setBounds(120, 25, 240, 21);
            panelOptions.add(eKeySecret);
        }

        JLabel lblNewLabel_2 = new JLabel("Password length:");
        lblNewLabel_2.setBounds(10, 58, 101, 15);
        panelOptions.add(lblNewLabel_2);
        {
            SpinnerModel modelLen = new SpinnerNumberModel(10, 0, 10, 1);
            ePassLen = new JSpinner(modelLen);
            ePassLen.setBounds(120, 55, 50, 21);
            ePassLen.setValue(10);
            panelOptions.add(ePassLen);
        }

        JLabel lblNewLabel_3 = new JLabel("Algorithm:");
        lblNewLabel_3.setBounds(10, 93, 94, 15);
        panelOptions.add(lblNewLabel_3);
        {
            cAlgorithm = new JComboBox<String>();
            cAlgorithm.setBounds(120, 90, 123, 23);
            cAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"Hmac", "Time"}));
            panelOptions.add(cAlgorithm);

            cAlgorithm.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (cAlgorithm.getSelectedIndex() == 0)
                    {
                        lInterval.setText("Counter:");
                        lHashAlgorithm.setEnabled(false);
                        cHashAlgorithm.setEnabled(false);
                        cHashAlgorithm.setSelectedIndex(0);
                        cUseBaseTime.setEnabled(false);
                        eBaseTime.setEnabled(false);
                    }
                    else
                    {
                        lInterval.setText("Time interval:");
                        lHashAlgorithm.setEnabled(true);
                        cHashAlgorithm.setEnabled(true);
                        cUseBaseTime.setEnabled(true);
                        eBaseTime.setEnabled(true);
                    }
                }
            });
        }

        JPanel panelAddOptions = new JPanel();
        panelAddOptions.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "", TitledBorder.LEADING, TitledBorder.TOP, null, null));
        panelAddOptions.setBounds(10, 130, 355, 120);
        panelOptions.add(panelAddOptions);
        panelAddOptions.setLayout(null);

        lInterval = new JLabel("Counter:");
        lInterval.setBounds(10, 23, 80, 15);
        panelAddOptions.add(lInterval);
        {
            SpinnerModel modelC = new SpinnerNumberModel(30, 0, 999, 1);
            eInterval = new JSpinner(modelC);
            eInterval.setBounds(110, 25, 50, 21);
            panelAddOptions.add(eInterval);
        }

        lHashAlgorithm = new JLabel("Hash algorithm:");
        lHashAlgorithm.setBounds(10, 58, 94, 15);
        lHashAlgorithm.setEnabled(false);
        panelAddOptions.add(lHashAlgorithm);
        {
            cHashAlgorithm = new JComboBox<String>();
            cHashAlgorithm.setBounds(110, 55, 123, 23);
            cHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"SHA-1 (default)", "SHA-256", "SHA-512"}));
            cHashAlgorithm.setEnabled(false);
            panelAddOptions.add(cHashAlgorithm);
        }

        cUseBaseTime = new JCheckBox("Use this time");
        cUseBaseTime.setBounds(5, 93, 100, 19);
        cUseBaseTime.setEnabled(false);
        panelAddOptions.add(cUseBaseTime);
        {
            Calendar cal = Calendar.getInstance();
            SpinnerModel modelT = new SpinnerDateModel(cal.getTime(), null, null, Calendar.MONTH);
            eBaseTime = new JSpinner(modelT);
            JSpinner.DateEditor de = new JSpinner.DateEditor(eBaseTime, "dd.MM.yyyy HH:mm:ss");
            eBaseTime.setEditor(de);
            eBaseTime.setBounds(110, 92, 155, 21);
            eBaseTime.setEnabled(false);
            panelAddOptions.add(eBaseTime);
        }

        JPanel panelValid = new JPanel();
        panelValid.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "", TitledBorder.LEADING, TitledBorder.TOP, null, null));
        panelValid.setBounds(5, 295, 375, 100);
        contentPanel.add(panelValid);
        panelValid.setLayout(null);

        JLabel lblNewLabel_4 = new JLabel("Delta:");
        lblNewLabel_4.setBounds(10, 18, 80, 15);
        panelValid.add(lblNewLabel_4);
        {
            SpinnerModel modelD = new SpinnerNumberModel(0, 0, 999, 1);
            eDelta = new JSpinner(modelD);
            eDelta.setBounds(120, 15, 50, 21);
            panelValid.add(eDelta);
        }

        JLabel lblNewLabel_5 = new JLabel("Password:");
        lblNewLabel_5.setBounds(10, 48, 80, 15);
        panelValid.add(lblNewLabel_5);
        {
            ePassword = new JTextField();
            ePassword.setBounds(120, 45, 150, 21);
            panelValid.add(ePassword);
        }

        bValidate = new JButton("Is password valid");
        bValidate.setBounds(220, 70, 150, 25);
        bValidate.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                if (eKeySecret.getText().length() == 0)
                    showErrorMessage("Empty Key secret");
                else if (ePassLen.getValue().toString().length() == 0)
                    showErrorMessage("Empty Password length");
                else
                    doValidate();
            }
        });
        panelValid.add(bValidate);
	}

    protected void doValidate() {
        Boolean Res = false;

        try {
            server.config("Delta=" + eDelta.getValue().toString());

            if (cAlgorithm.getSelectedIndex() == 0) {
                Res = server.isHOTPPasswordValid(eKeySecret.getText().getBytes(StandardCharsets.US_ASCII), (Integer) ePassLen.getValue(), (Integer) eInterval.getValue(), ePassword.getText());
            } else {
                if (cUseBaseTime.isSelected()) {
                    server.config("BaseTime=" + new SimpleDateFormat("dd.MM.yyyy HH:mm:ss").format((Date) eBaseTime.getValue()));
                } else {
                    server.config("BaseTime=");
                }

                String HashAlgorithm;
                switch (cHashAlgorithm.getSelectedIndex()) {
                    case 1:
                        HashAlgorithm = "SHA256";
                        break;
                    case 2:
                        HashAlgorithm = "SHA512";
                        break;
                    default:
                        HashAlgorithm = "SHA1";
                        break;
                }

                Res = server.isTOTPPasswordValid(eKeySecret.getText().getBytes(StandardCharsets.US_ASCII), (Integer) ePassLen.getValue(), (Integer) eInterval.getValue(), HashAlgorithm, ePassword.getText());
            }
        }
        catch (Exception e)
        {}

        if (Res)
        {
            showMessage("Password is valid");
        }
        else
        {
            showErrorMessage("Password is not valid");
        }
    }

    static void showMessage(String msg){
        JOptionPane.showMessageDialog(null, msg, "Info", JOptionPane.INFORMATION_MESSAGE);
    }

    static void showErrorMessage(String msg){
        JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
    }

    public void actionPerformed(ActionEvent e) {

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



