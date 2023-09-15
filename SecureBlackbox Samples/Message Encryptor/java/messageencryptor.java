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
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

class messageencryptor extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;

	private JTextField edCertFile;
	private JTextField edCertPassword;
	private JComboBox<String> cmbEncryptionAlgorithm;

	Messageencryptor encryptor;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			messageencryptor dialog = new messageencryptor();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public messageencryptor()
	{
		encryptor = new Messageencryptor();


		setTitle("Message Encryptor");
		
		setBounds(100, 100, 500, 300);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);

		contentPanel.setLayout(null);JLabel lblCaption = new JLabel("This sample illustrates how to create encrypted ('enveloped') PKCS#7 messages.");
		lblCaption.setBounds(10, 5, 490, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblInputFile = new JLabel("Input File");
		lblInputFile.setBounds(10, 33, 70, 14);
		contentPanel.add(lblInputFile);

		edInputFile = new JTextField();
		edInputFile.setBounds(80, 30, 310, 20);
		contentPanel.add(edInputFile);
		edInputFile.setColumns(10);

		JButton sbBrowseInputFile = new JButton("Browse");
		sbBrowseInputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edInputFile.setText(getFileName());
			}
		});
		sbBrowseInputFile.setBounds(397, 28, 80, 25);
		contentPanel.add(sbBrowseInputFile);
		contentPanel.setLayout(null);

		JLabel lblOutputPath = new JLabel("Output File");
		lblOutputPath.setBounds(10, 63, 70, 14);
		contentPanel.add(lblOutputPath);

		edOutputFile = new JTextField();
		edOutputFile.setBounds(80, 60, 310, 20);
		contentPanel.add(edOutputFile);
		edOutputFile.setColumns(10);

		JButton sbBrowseOutputFile = new JButton("Browse");
		sbBrowseOutputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edOutputFile.setText(getSaveFileName());
			}
		});
		sbBrowseOutputFile.setBounds(397, 58, 80, 25);
		contentPanel.add(sbBrowseOutputFile);
		contentPanel.setLayout(null);

		JPanel panelOptions = new JPanel();
		panelOptions.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Encripting options  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panelOptions.setBounds(5, 90, 475, 130);
		contentPanel.add(panelOptions);
		panelOptions.setLayout(null);

		JLabel lbSymmetricAlgorithm = new JLabel("Encryption algorithm");
		lbSymmetricAlgorithm.setBounds(10, 23, 125, 14);
		panelOptions.add(lbSymmetricAlgorithm);

		cmbEncryptionAlgorithm = new JComboBox<String>();
		cmbEncryptionAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"3DES", "RC4", "RC2", "AES128", "AES192", "AES256", "Twofish128"}));
		cmbEncryptionAlgorithm.setBounds(145, 20, 140, 22);
		panelOptions.add(cmbEncryptionAlgorithm);

		JLabel lblSignCertFile = new JLabel("Encryption certificate:");
		lblSignCertFile.setBounds(10, 53, 150, 14);
		panelOptions.add(lblSignCertFile);

		edCertFile = new JTextField();
		edCertFile.setBounds(10, 70, 365, 20);
		panelOptions.add(edCertFile);
		edCertFile.setColumns(10);

		JButton sbBrowseRecipientCertFile = new JButton("Browse");
		sbBrowseRecipientCertFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edCertFile.setText(getSaveFileName());
			}
		});
		sbBrowseRecipientCertFile.setBounds(385, 68, 80, 25);
		panelOptions.add(sbBrowseRecipientCertFile);

		JLabel lblSignCertPassword = new JLabel("Certificate password:");
		lblSignCertPassword.setBounds(10, 98, 130, 14);
		panelOptions.add(lblSignCertPassword);

		edCertPassword = new JTextField();
		edCertPassword.setBounds(140, 95, 235, 20);
		panelOptions.add(edCertPassword);
		edCertPassword.setColumns(10);

		JButton btnEncrypt = new JButton("Encrypt");
		btnEncrypt.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				encrypt();
			}
		});
		btnEncrypt.setBounds(397, 230, 80, 25);
		contentPanel.add(btnEncrypt);

		cmbEncryptionAlgorithm.setSelectedIndex(0);
	}

	protected void encrypt()
	{
		try
		{
			encryptor.setInputFile(edInputFile.getText());
			encryptor.setOutputFile(edOutputFile.getText());

			encryptor.setEncryptionAlgorithm(cmbEncryptionAlgorithm.getItemAt(cmbEncryptionAlgorithm.getSelectedIndex()));

			encryptor.setEncryptionCertificate(LoadCertificate(edCertFile.getText(), edCertPassword.getText()));

			encryptor.encrypt();

			showMessage("Info", "The file successfully encrypted");
		}
		catch (Exception ex)
		{
			showErrorMessage(ex.getMessage());
		}
	}	

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}
	
	static void showMessage(String caption, String msg) {
		JOptionPane.showMessageDialog(null, msg, caption, JOptionPane.INFORMATION_MESSAGE);
	}	

	public Certificate LoadCertificate(String file, String password)
	{
		Certificate cert = null;

		if (file.length() > 0)
		{
			try
			{
				Certificatemanager manager = new Certificatemanager();
				manager.importFromFile(file, password);
				cert = manager.getCertificate();
			}
			catch (Exception e)
			{
				showErrorMessage("Cannot load certificate!");
			}
		}

		return cert;
	}

	String getFileName(){
		JFileChooser fc = new JFileChooser();

		int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	String getSaveFileName(){
		JFileChooser fc = new JFileChooser();

		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
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



