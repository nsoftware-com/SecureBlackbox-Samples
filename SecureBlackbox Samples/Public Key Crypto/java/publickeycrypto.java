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
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.File;

import secureblackbox.*;

import static secureblackbox.Publickeycrypto.*;

public class publickeycrypto extends JDialog {

	private static final long serialVersionUID = 1L;
	private ButtonGroup buttonGroup = new ButtonGroup();
	private JRadioButton rbSign;
	private JRadioButton rbVerify;
	private JTextField tbInputFile;
	private JLabel lblSignatureFilename;
	private JTextField tbSignatureFile;
	private JComboBox<String> comboEncoding;
	private JComboBox<String> comboKeyContainerType;
	private JLabel lblKeyAlg;
	private JComboBox<String> comboKeyAlg;
	private JLabel lblCurve;
	private JComboBox<String> comboCurve;
	private JTextField tbKeyFile;
	private JPasswordField tbPassphrase;
	private JButton btnGo;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					publickeycrypto dialog = new publickeycrypto();
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
	public publickeycrypto() {
		setTitle("Public key crypto Demo");
		setBounds(100, 100, 555, 420);
		getContentPane().setLayout(null);

		JLabel lblCaption = new JLabel("This sample shows how to detached sign and verify signature.");
		lblCaption.setBounds(10, 5, 490, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		getContentPane().add(lblCaption);

		JPanel panel = new JPanel();
		panel.setBorder(new TitledBorder(null, "Settings", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel.setBounds(10, 26, 523, 310);
		getContentPane().add(panel);
		panel.setLayout(null);

		rbSign = new JRadioButton("Sign");
		buttonGroup.add(rbSign);
		rbSign.setBounds(10, 25, 70, 14);
		rbSign.setSelected(true);
		rbSign.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				if (rbSign.isSelected())
				{
					lblSignatureFilename.setText("Output filename:");
					btnGo.setText("Sign");
				}
				else
				{
					lblSignatureFilename.setText("Signature filename:");
					btnGo.setText("Verify");
				}
			}
		});
		panel.add(rbSign);

		rbVerify = new JRadioButton("Verify");
		buttonGroup.add(rbVerify);
		rbVerify.setBounds(100, 25, 70, 14);
		rbVerify.addChangeListener(new ChangeListener() {
			@Override
			public void stateChanged(ChangeEvent e) {
				if (rbSign.isSelected())
				{
					lblSignatureFilename.setText("Output filename:");
					btnGo.setText("Sign");
				}
				else
				{
					lblSignatureFilename.setText("Signature filename:");
					btnGo.setText("Verify");
				}
			}
		});
		panel.add(rbVerify);

		JLabel lblInputFilename = new JLabel("Input filename:");
		lblInputFilename.setBounds(10, 52, 246, 14);
		panel.add(lblInputFilename);
		
		tbInputFile = new JTextField();
		tbInputFile.setBounds(10, 67, 400, 20);
		panel.add(tbInputFile);
		tbInputFile.setColumns(10);
		
		JButton btnBrowseInput = new JButton("Browse");
		btnBrowseInput.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tbInputFile.setText(getOpenFileName());
			}
		});
		btnBrowseInput.setBounds(422, 65, 91, 23);
		panel.add(btnBrowseInput);
		
		lblSignatureFilename = new JLabel("Output filename:");
		lblSignatureFilename.setBounds(10, 92, 174, 14);
		panel.add(lblSignatureFilename);

		tbSignatureFile = new JTextField();
		tbSignatureFile.setColumns(10);
		tbSignatureFile.setBounds(10, 107, 400, 20);
		panel.add(tbSignatureFile);
		
		JButton buttonBrowseOutput = new JButton("Browse");
		buttonBrowseOutput.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (rbSign.isSelected())
					tbSignatureFile.setText(getSaveFileName());
				else
					tbSignatureFile.setText(getOpenFileName());
			}
		});
		buttonBrowseOutput.setBounds(422, 105, 91, 23);
		panel.add(buttonBrowseOutput);
		
		JLabel lblEncoding = new JLabel("Encoding:");
		lblEncoding.setBounds(10, 137, 86, 14);
		panel.add(lblEncoding);

		comboEncoding = new JComboBox<String>();
		comboEncoding.setModel(new DefaultComboBoxModel<String>(new String[] {"Binary", "Base64", "Compact", "JSON"}));
		comboEncoding.setBounds(10, 152, 91, 22);
		panel.add(comboEncoding);

		JLabel lblKeyContainerFile = new JLabel("Key container type:");
		lblKeyContainerFile.setBounds(10, 182, 162, 14);
		panel.add(lblKeyContainerFile);

		comboKeyContainerType = new JComboBox<String>();
		comboKeyContainerType.setModel(new DefaultComboBoxModel<String>(new String[] {"Generic private key", "X.509 certificate"}));
		comboKeyContainerType.setBounds(10, 197, 162, 22);
		comboKeyContainerType.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (comboKeyContainerType.getSelectedIndex() == 0)
				{
					lblKeyAlg.setEnabled(true);
					comboKeyAlg.setEnabled(true);
					comboKeyAlg.setSelectedIndex(0);
				}
				else
				{
					lblKeyAlg.setEnabled(false);
					comboKeyAlg.setEnabled(false);
					comboKeyAlg.setSelectedIndex(0);
				}
			}
		});
		panel.add(comboKeyContainerType);

		lblKeyAlg = new JLabel("Key algorithm:");
		lblKeyAlg.setBounds(200, 182, 100, 14);
		panel.add(lblKeyAlg);

		comboKeyAlg = new JComboBox<String>();
		comboKeyAlg.setModel(new DefaultComboBoxModel<String>(new String[] {"", "RSA", "DSA", "EC", "ECDSA", "DH", "EDDSA"}));
		comboKeyAlg.setBounds(200, 197, 100, 22);
		comboKeyAlg.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (comboKeyAlg.getItemAt(comboKeyAlg.getSelectedIndex()) == "EC")
				{
					lblCurve.setEnabled(comboKeyAlg.isEnabled());
					comboCurve.setEnabled(comboKeyAlg.isEnabled());
					comboCurve.setSelectedIndex(0);
				}
				else
				{
					lblCurve.setEnabled(false);
					comboCurve.setEnabled(false);
					comboCurve.setSelectedIndex(0);
				}
			}
		});
		panel.add(comboKeyAlg);

		lblCurve = new JLabel("EC curve:");
		lblCurve.setBounds(340, 182, 100, 14);
		panel.add(lblCurve);

		comboCurve = new JComboBox<String>();
		comboCurve.setModel(new DefaultComboBoxModel<String>(new String[] {"", "SECP112R1", "SECT113R1", "SECP128R1", "SECT131R1", "SECP160K1", "SECT163K1", "C2PNB176W1", "C2TNB191V1", "SECP192K1", "SECT193R1", "C2PNB208W1", "SECP224K1", "SECT233K1", "SECT239K1", "SECP256K1", "C2PNB272W1", "SECT283K1", "C2PNB304W1", "C2TNB359V1", "C2PNB368W1", "SECP384R1", "SECT409K1", "C2TNB431R1", "BRAINPOOLP512R1", "SECP521R1", "SECT571K1"}));
		comboCurve.setBounds(340, 197, 100, 22);
		panel.add(comboCurve);

		JLabel lblPrivateKeyContainer = new JLabel("Private key container file:");
		lblPrivateKeyContainer.setBounds(10, 222, 174, 14);
		panel.add(lblPrivateKeyContainer);

		tbKeyFile = new JTextField();
		tbKeyFile.setColumns(10);
		tbKeyFile.setBounds(10, 237, 400, 20);
		panel.add(tbKeyFile);
		
		JButton buttonBrowsePrivateKey = new JButton("Browse");
		buttonBrowsePrivateKey.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tbKeyFile.setText(getOpenFileName());
			}
		});
		buttonBrowsePrivateKey.setBounds(422, 235, 91, 23);
		panel.add(buttonBrowsePrivateKey);
		
		JLabel lblPassword = new JLabel("Password");
		lblPassword.setBounds(10, 262, 136, 14);
		panel.add(lblPassword);
		
		tbPassphrase = new JPasswordField();
		tbPassphrase.setBounds(10, 277, 136, 20);
		panel.add(tbPassphrase);
		
		btnGo = new JButton("Sign");
		btnGo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!new File(tbInputFile.getText()).exists())
					showMessage("Source file not found");
				else if (tbSignatureFile.getText().compareTo("") == 0)
					showMessage("Please, select output file");
				else if (!new File(tbKeyFile.getText()).exists())
					showMessage("Key or certificate file not found");
				else
				{
					if (rbSign.isSelected())
					{
						DoSignDetached();
					}
					else
					{
						if (!new File(tbSignatureFile.getText()).exists())
						{
							showMessage("Signature file not found");
						}
						else
						{
							DoVerifyDetached();
						}
					}
				}
			}
		});
		btnGo.setBounds(440, 350, 91, 23);
		getContentPane().add(btnGo);

		comboEncoding.setSelectedIndex(0);
		comboKeyContainerType.setSelectedIndex(0);
		comboKeyAlg.setSelectedIndex(0);
	}

	private void LoadKeyFromFile(Publickeycrypto crypto)
	{
		Cryptokeymanager keymanager = new Cryptokeymanager();

		try {
			keymanager.addCryptokeymanagerEventListener(new CryptokeymanagerEventListener() {
				@Override
				public void error(CryptokeymanagerErrorEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void passwordNeeded(CryptokeymanagerPasswordNeededEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(CryptokeymanagerNotificationEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void supercoreIntercept(CryptokeymanagerSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
				}
			});

		if (comboKeyContainerType.getSelectedIndex() == 1)
		{
			Certificatemanager certmanager = new Certificatemanager();

			certmanager.importFromFile(tbKeyFile.getText(), tbPassphrase.getText());

			keymanager.setCertificate(certmanager.getCertificate());
			keymanager.importFromCert();

			crypto.setKey(keymanager.getKey());
		}
		else
		{
			keymanager.importFromFile(tbKeyFile.getText(), secureblackbox.Constants.kffAuto, comboKeyAlg.getItemAt(comboKeyAlg.getSelectedIndex()), comboCurve.getItemAt(comboCurve.getSelectedIndex()), "", secureblackbox.Constants.ktAuto);

			crypto.setKey(keymanager.getKey());
		}
		}
		catch(Exception e)
		{
			showErrorMessage(e.getMessage());
		}
	}

	private void DoSignDetached()
	{
		Publickeycrypto crypto = new Publickeycrypto();
		try
		{
			switch (comboEncoding.getSelectedIndex())
			{
				case 1: crypto.setOutputEncoding(cetBase64); break;
				case 2: crypto.setOutputEncoding(cetCompact); break;
				case 3: crypto.setOutputEncoding(cetJSON); break;
				default: crypto.setOutputEncoding(cetBinary); break;
			}

			// loading key
			LoadKeyFromFile(crypto);

			// signing input data
			crypto.signFile(tbInputFile.getText(), tbSignatureFile.getText(), true);

			showMessage("The file was signed successfully", "Succes");
		}
		catch(Exception e)
		{
			showErrorMessage(e.getMessage());
		}
	}

	private void DoVerifyDetached()
	{
		Publickeycrypto crypto = new Publickeycrypto();
		try
		{
			switch (comboEncoding.getSelectedIndex())
			{
				case 1: crypto.setInputEncoding(cetBase64); break;
				case 2: crypto.setInputEncoding(cetCompact); break;
				case 3: crypto.setInputEncoding(cetJSON); break;
				default: crypto.setInputEncoding(cetBinary); break;
			}

			// loading key
			LoadKeyFromFile(crypto);

			// verifying input data
			crypto.verifyDetachedFile(tbInputFile.getText(), tbSignatureFile.getText());

			switch (crypto.getSignatureValidationResult())
			{
				case svtValid: showMessage("Verification succeeded", "Succes"); break;
				case svtCorrupted: showErrorMessage("Verification corrupted"); break;
				case svtFailure: showErrorMessage("Verification failed"); break;
				default: showErrorMessage("Verification unknown"); break;
			}
		}
		catch(Exception e)
		{
			showErrorMessage(e.getMessage());
		}
	}

	private void showMessage(String msg) {
		JOptionPane.showMessageDialog(null, msg, getTitle(), JOptionPane.INFORMATION_MESSAGE);
	}

	private void showMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.INFORMATION_MESSAGE);
	}

	private void showErrorMessage(String msg) {
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}
	
	protected String getSaveFileName() {
		JFileChooser fc = new JFileChooser();
			    
	    int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	}
	
	protected String getOpenFileName() {
		JFileChooser fc = new JFileChooser();
			    
	    int returnVal = fc.showOpenDialog(this);
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



