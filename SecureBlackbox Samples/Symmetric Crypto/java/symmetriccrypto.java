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
import java.awt.EventQueue;

import javax.swing.*;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.nio.charset.StandardCharsets;

import secureblackbox.*;
import static secureblackbox.Symmetriccrypto.*;

public class symmetriccrypto extends JDialog {

	private static final long serialVersionUID = 1L;
	private JComboBox<String> cbEncoding;
	private JTextField tbInput;
	private JPasswordField tbPassword;
	private JTextField tbEncrypted;
	private JTextField tbDecrypted;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					symmetriccrypto dialog = new symmetriccrypto();
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
	public symmetriccrypto() {
		setResizable(false);
		setTitle("Symmetric crypto Demo");
		setBounds(100, 100, 340, 350);
		getContentPane().setLayout(null);

		JLabel lblEncoding = new JLabel("Encoding:");
		lblEncoding.setBounds(10, 15, 115, 14);
		getContentPane().add(lblEncoding);

		cbEncoding = new JComboBox<String>();
		cbEncoding.setBounds(10, 33, 123, 23);
		cbEncoding.setModel(new DefaultComboBoxModel<String>(new String[] {"Binary", "Base64", "Compact", "JSON"}));
		getContentPane().add(cbEncoding);

		JLabel lblInputString = new JLabel("Input string:");
		lblInputString.setBounds(10, 70, 115, 14);
		getContentPane().add(lblInputString);
		
		tbInput = new JTextField();
		tbInput.setBounds(10, 87, 314, 20);
		getContentPane().add(tbInput);
		tbInput.setColumns(10);
		
		JLabel lblPassword = new JLabel("Password:");
		lblPassword.setBounds(10, 120, 131, 14);
		getContentPane().add(lblPassword);
		
		tbPassword = new JPasswordField();
		tbPassword.setBounds(10, 137, 314, 20);
		getContentPane().add(tbPassword);
		
		JLabel lblEncryptedOutput = new JLabel("Encrypted output:");
		lblEncryptedOutput.setBounds(10, 170, 154, 14);
		getContentPane().add(lblEncryptedOutput);
		
		tbEncrypted = new JTextField();
		tbEncrypted.setBounds(10, 187, 314, 20);
		getContentPane().add(tbEncrypted);
		tbEncrypted.setColumns(10);
		
		JLabel lblDecryptedString = new JLabel("Decrypted string:");
		lblDecryptedString.setBounds(10, 220, 165, 14);
		getContentPane().add(lblDecryptedString);
		
		tbDecrypted = new JTextField();
		tbDecrypted.setEnabled(false);
		tbDecrypted.setBounds(10, 237, 314, 20);
		getContentPane().add(tbDecrypted);
		tbDecrypted.setColumns(10);
		
		JButton btnEncrypt = new JButton("Encrypt");
		btnEncrypt.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				encryptClick();
			}
		});
		btnEncrypt.setBounds(66, 270, 91, 23);
		getContentPane().add(btnEncrypt);
		
		JButton btnDecrypt = new JButton("Decrypt");
		btnDecrypt.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				decryptClick();
			}
		});
		btnDecrypt.setBounds(167, 270, 91, 23);
		getContentPane().add(btnDecrypt);

	}

	protected void encryptClick() {
		try
		{
			Symmetriccrypto crypto = new Symmetriccrypto();

			crypto.setEncryptionAlgorithm("AES256");

			switch (cbEncoding.getSelectedIndex())
			{
				case 1: crypto.setOutputEncoding(cetBase64); break;
				case 2: crypto.setOutputEncoding(cetCompact); break;
				case 3: crypto.setOutputEncoding(cetJSON); break;
				default: crypto.setOutputEncoding(cetBinary); break;
			}

			PasswordToKey(new String(tbPassword.getPassword()), crypto);

			byte[] inBuf = tbInput.getText().getBytes(StandardCharsets.ISO_8859_1);

			byte[] outBuf =  crypto.encrypt(inBuf);

			tbEncrypted.setText(new String(outBuf, StandardCharsets.ISO_8859_1));
		}
		catch (Exception ex)
		{
			showErrorMessage("Encryption error: " + ex.getMessage(), "Error");
		}
	}

	protected void decryptClick() {
		try
		{
			Symmetriccrypto crypto = new Symmetriccrypto();

			crypto.setEncryptionAlgorithm("AES256");

			switch (cbEncoding.getSelectedIndex())
			{
				case 1: crypto.setInputEncoding(cetBase64); break;
				case 2: crypto.setInputEncoding(cetCompact); break;
				case 3: crypto.setInputEncoding(cetJSON); break;
				default: crypto.setInputEncoding(cetBinary); break;
			}

			PasswordToKey(new String(tbPassword.getPassword()), crypto);

			byte[] inBuf = tbEncrypted.getText().getBytes(StandardCharsets.ISO_8859_1);

			byte[] outBuf =  crypto.decrypt(inBuf);

			tbDecrypted.setText(new String(outBuf, StandardCharsets.ISO_8859_1));
		}
		catch (Exception ex)
		{
			showErrorMessage("Decryption error: " + ex.getMessage(), "Error");
		}
	}

	static void showErrorMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.ERROR_MESSAGE);
	}

	private void PasswordToKey(String pass, Symmetriccrypto crypto)
	{
		Cryptokeymanager keymanager = new Cryptokeymanager();

		int KeyBits = 256;

		try {
			keymanager.deriveKey(KeyBits, pass, ""); // derive 256-bit key

			keymanager.getKey().setIV(new byte[16]); // set 128-bit initialization vector

			crypto.setKey(keymanager.getKey());
		}
		catch (Exception ex)
		{
			showErrorMessage("Generate key error: " + ex.getMessage(), "Error");
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



