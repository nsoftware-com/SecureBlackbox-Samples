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
/*
 * SecureBlackbox 2022 Java Edition- Demo Application
 *
 * Copyright (c) 2022 /n software inc. - All rights reserved. - www.nsoftware.com
 *
 */
import java.awt.EventQueue;

import javax.swing.*;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.nio.charset.StandardCharsets;

import secureblackbox.*;
import static secureblackbox.Symmetriccrypto.*;

public class hashfunction extends JDialog {

	private static final long serialVersionUID = 1L;
	private JComboBox<String> cbEncoding;
	private JTextField tbInput;
	private JPasswordField tbPassword;
	private JTextField tbHash;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					hashfunction dialog = new hashfunction();
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
	public hashfunction() {
		setResizable(false);
		setTitle("Hash function Demo");
		setBounds(100, 100, 340, 310);
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
		
		JLabel lblEncryptedOutput = new JLabel("Hash output:");
		lblEncryptedOutput.setBounds(10, 170, 154, 14);
		getContentPane().add(lblEncryptedOutput);

		tbHash = new JTextField();
		tbHash.setBounds(10, 187, 314, 20);
		getContentPane().add(tbHash);
		tbHash.setColumns(10);

		JButton btnEncrypt = new JButton("Hash");
		btnEncrypt.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				hashClick();
			}
		});
		btnEncrypt.setBounds(124, 237, 91, 23);
		getContentPane().add(btnEncrypt);
	}

	protected void hashClick() {
		try
		{
			Hashfunction hash = new Hashfunction();

			hash.setAlgorithm("SHA256");

			switch (cbEncoding.getSelectedIndex())
			{
				case 1: hash.setOutputEncoding(cetBase64); break;
				case 2: hash.setOutputEncoding(cetCompact); break;
				case 3: hash.setOutputEncoding(cetJSON); break;
				default: hash.setOutputEncoding(cetBinary); break;
			}

			String pass = new String(tbPassword.getPassword());
			if (!pass.isEmpty())
				PasswordToKey(pass, hash);

			byte[] inBuf = tbInput.getText().getBytes(StandardCharsets.ISO_8859_1);

			byte[] outBuf =  hash.hash(inBuf);

			tbHash.setText(new String(outBuf, StandardCharsets.ISO_8859_1));
		}
		catch (Exception ex)
		{
			showErrorMessage("Hash error: " + ex.getMessage(), "Error");
		}
	}

	static void showErrorMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.ERROR_MESSAGE);
	}

	private void PasswordToKey(String pass, Hashfunction hash)
	{
		Cryptokeymanager keymanager = new Cryptokeymanager();

		try {
			keymanager.deriveKey(256, pass, "");

			hash.setKey(keymanager.getKey());
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



