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
import java.awt.event.*;
import java.io.File;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import secureblackbox.*;

public class pgpreader extends JDialog {

	class ItemKey {
		public String Name;
		public PGPKey Value;

		public ItemKey(String name, PGPKey value) {
			this.Name = name;
			this.Value = value;
		}

		public String toString() {
			return this.Name;
		}
	}

	private static final long serialVersionUID = 1L;
	private Pgpreader reader;
	private Pgpkeyring keyring;

	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;
	private JCheckBox cbManualKeySelect;
	private JComboBox<ItemKey> decryptKeys;
	private JComboBox<ItemKey> verifyKeys;
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					pgpreader dialog = new pgpreader();
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
	public pgpreader() {
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				close();
			}
		});

		setResizable(false);
		setTitle("PGPReader Demo Application");

		setBounds(100, 100, 800, 320);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample is a younger PGP Desktop brother that illustrates the use of PGPReader component for processing protected OpenPGP files.");
		lblCaption.setBounds(10, 5, 790, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblInputFile = new JLabel("File to decrypt and verify");
		lblInputFile.setBounds(10, 33, 150, 14);
		contentPanel.add(lblInputFile);

		edInputFile = new JTextField();
		edInputFile.setBounds(160, 30, 380, 20);
		contentPanel.add(edInputFile);
		edInputFile.setColumns(10);

		JButton sbBrowseInputFile = new JButton("Browse");
		sbBrowseInputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edInputFile.setText(getOpenFileName());
			}
		});
		sbBrowseInputFile.setBounds(550, 28, 80, 25);
		contentPanel.add(sbBrowseInputFile);
		contentPanel.setLayout(null);

		JLabel lblOutputPath = new JLabel("Output File");
		lblOutputPath.setBounds(10, 63, 70, 14);
		contentPanel.add(lblOutputPath);

		edOutputFile = new JTextField();
		edOutputFile.setBounds(80, 60, 460, 20);
		contentPanel.add(edOutputFile);
		edOutputFile.setColumns(10);

		JButton sbBrowseOutputFile = new JButton("Browse");
		sbBrowseOutputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edOutputFile.setText(getSaveFileName());
			}
		});
		sbBrowseOutputFile.setBounds(550, 58, 80, 25);
		contentPanel.add(sbBrowseOutputFile);
		contentPanel.setLayout(null);

		JPanel panelOptions = new JPanel();
		panelOptions.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Keys  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panelOptions.setBounds(5, 90, 785, 160);
		contentPanel.add(panelOptions);
		panelOptions.setLayout(null);

		cbManualKeySelect = new JCheckBox("Manual key selection");
		cbManualKeySelect.setBounds(15, 25, 250, 14);
		cbManualKeySelect.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				decryptKeys.setEnabled(cbManualKeySelect.isSelected());
				verifyKeys.setEnabled(cbManualKeySelect.isSelected());
			}
		});
		panelOptions.add(cbManualKeySelect);

		JButton btnBrowseKeyring= new JButton("Browse keyring files...");
		btnBrowseKeyring.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				LoadKeyring();
			}
		});
		btnBrowseKeyring.setBounds(610, 25, 160, 25);
		panelOptions.add(btnBrowseKeyring);

		JLabel lblDecryptKeys = new JLabel("Decryption key:");
		lblDecryptKeys.setBounds(10, 75, 100, 14);
		panelOptions.add(lblDecryptKeys);

		decryptKeys = new JComboBox<ItemKey>();
		decryptKeys.setBounds(110, 73, 660, 23);
		decryptKeys.setEnabled(false);
		panelOptions.add(decryptKeys);

		JLabel lblVerifyKeys = new JLabel("Verifying key:");
		lblVerifyKeys.setBounds(10, 115, 100, 14);
		panelOptions.add(lblVerifyKeys);

		verifyKeys = new JComboBox<ItemKey>();
		verifyKeys.setBounds(110, 113, 660, 23);
		verifyKeys.setEnabled(false);
		panelOptions.add(verifyKeys);

		JPanel buttonPane = new JPanel();
		buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
		getContentPane().add(buttonPane, BorderLayout.SOUTH);

		JButton okButton = new JButton("Decrypt and verify");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				DecryptAndVerify();
			}
		});
		okButton.setActionCommand("Decrypt and verify");
		buttonPane.add(okButton);
		getRootPane().setDefaultButton(okButton);

		init();
	}

	protected void close()
	{
		try
		{
			reader.dispose();
			keyring.dispose();
		}
		catch (Exception e)
		{}
		dispose();
		System.exit(0);
	}

	private void init() {
		reader = new Pgpreader();
		keyring = new Pgpkeyring();
		try
		{
			reader.addPgpreaderEventListener(new PgpreaderEventListener() {
				@Override
				public void encryptionInfo(PgpreaderEncryptionInfoEvent e) {

				}

				@Override
				public void error(PgpreaderErrorEvent e) {

				}

				@Override
				public void externalDecrypt(PgpreaderExternalDecryptEvent e) {

				}

				@Override
				public void fileExtractionStart(PgpreaderFileExtractionStartEvent e) {

				}

				@Override
				public void keyPassphraseNeeded(PgpreaderKeyPassphraseNeededEvent e) {
					e.passphrase = requestPassword(GetDefaultUserID(e.userID, e.keyID));
					e.skip = (e.passphrase == "");
				}

				@Override
				public void multipleFilesFound(PgpreaderMultipleFilesFoundEvent e) {
					e.proceed = chooseMessage(e.tarFilename + " file found inside the encrypted file\n" +
							"Do you want to extract it after decryption or decrypt as is?");
				}

				@Override
				public void notification(PgpreaderNotificationEvent e) {

				}

				@Override
				public void passphraseNeeded(PgpreaderPassphraseNeededEvent e) {

				}

				@Override
				public void progress(PgpreaderProgressEvent e) {

				}

				@Override
				public void signed(PgpreaderSignedEvent e) {

				}

				@Override
				public void supercoreIntercept(PgpreaderSupercoreInterceptEvent e) {

				}
			});
		}
		catch (Exception e)
		{}
	}

	private String GetDefaultUserID(String username, String keyID)
	{
		String result;
		if (username != "")
		{
			result = username;
		}
		else
		{
			result = "No name";
		}
		result = result + " [0x" + keyID + "]";
		return result;
	}

	private String GetDefaultUserID(PGPKey key)
	{
		if (key.getIsSubkey())
			return "[SUB] " + GetDefaultUserID(key.getUsername(), key.getKeyID());
		else
			return "[PRI] " + GetDefaultUserID(key.getUsername(), key.getKeyID());
	}

	private void PopulateKeyLists()
	{
		decryptKeys.removeAllItems();
		verifyKeys.removeAllItems();

		decryptKeys.addItem(new ItemKey("", null));
		for (int i = 0; i < keyring.getSecretKeys().size(); i++)
		{
			decryptKeys.addItem(new ItemKey(GetDefaultUserID(keyring.getSecretKeys().item(i)), keyring.getSecretKeys().item(i)));
		}

		verifyKeys.addItem(new ItemKey("", null));
		for (int i = 0; i < keyring.getPublicKeys().size(); i++)
		{
			verifyKeys.addItem(new ItemKey(GetDefaultUserID(keyring.getPublicKeys().item(i)), keyring.getPublicKeys().item(i)));
		}
	}

	private void LoadKeyring()
	{
		loadkeyringdialog dlg = new loadkeyringdialog();
		dlg.setTitle("Load keyring");
		dlg.setModal(true);
		dlg.setVisible(true);
		if (dlg.isOK()) 
		{
			try 
			{
				if (keyring.isOpened())
					keyring.close();

				keyring.load(dlg.getPublicKeyringText(), dlg.getSecretKeyringText());
			} 
			catch(Exception ex) 
			{
				showErrorMessage(ex.getMessage(), "Keyring error");
				return;
			}

			PopulateKeyLists();
		}
	}

	private void DecryptAndVerify()
	{
		File f = new File(edInputFile.getText());
		String fileName = f.getName();
		if (!(f.exists() && !f.isDirectory())) {
			showErrorMessage("Input file does not exist", "Error");
		}
		else
		if (edOutputFile.getText().isEmpty()) {
			showErrorMessage("Output file does not specified", "Error");
		}
		else
		if (keyring.getPublicKeys().size() == 0) {
			showErrorMessage("Your keyring does not contain public keys", "Error");
		}
		else
		if (keyring.getSecretKeys().size() == 0) {
			showErrorMessage("Your keyring does not contain secret keys", "Error");
		}
		else
		if (cbManualKeySelect.isSelected() && decryptKeys.getSelectedIndex() <= 0) {
			showErrorMessage("Please select the decryption key", "Error");
		}
		else
		if (cbManualKeySelect.isSelected() && verifyKeys.getSelectedIndex() <= 0) {
			showErrorMessage("Please select the verifying key", "Error");
		}
		else
		{
			try
			{
				if (cbManualKeySelect.isSelected())
				{
					reader.getDecryptingKeys().clear();
					reader.getDecryptingKeys().add(((ItemKey) decryptKeys.getSelectedItem()).Value);
					reader.getVerifyingKeys().clear();
					reader.getVerifyingKeys().add(((ItemKey) verifyKeys.getSelectedItem()).Value);
				}
				else
				{
					reader.getDecryptingKeys().clear();
					for (int i = 0; i < keyring.getSecretKeys().size(); i++)
					{
						if (!keyring.getSecretKeys().item(i).getIsSubkey())
						{
							reader.getDecryptingKeys().add(keyring.getSecretKeys().item(i));
						}
					}
					reader.getVerifyingKeys().clear();
					for (int i = 0; i < keyring.getPublicKeys().size(); i++)
					{
						if (!keyring.getPublicKeys().item(i).getIsSubkey())
						{
							reader.getVerifyingKeys().add(keyring.getPublicKeys().item(i));
						}
					}

				}

				reader.decryptAndVerifyFile(edInputFile.getText(), edOutputFile.getText());

				showSignaturesDialog();

				showMessage("The file was decrypted successfully", "Information");
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage(), "Error");
			}
			close();
		}
	}

	public void showSignaturesDialog()
	{
		signaturesdialog frmSignatures = new signaturesdialog(reader.getSignatures(), keyring);
		frmSignatures.setLocationRelativeTo(this);
		frmSignatures.setModal(true);
		frmSignatures.setVisible(true);

		frmSignatures.dispose();
	}

	private void showErrorMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.ERROR_MESSAGE);				
	}

	private void showMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.INFORMATION_MESSAGE);
	}

	private boolean chooseMessage(String msg) {
		return (JOptionPane.showConfirmDialog(this, msg) == JOptionPane.YES_OPTION);
	}

	protected String getOpenFileName() {
		JFileChooser fc = new JFileChooser();
	    int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	}

	protected String getSaveFileName() {
		JFileChooser fc = new JFileChooser();
	    int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	}

 	private String requestPassword(String msg) {
		JPasswordField jpf = new JPasswordField();
		int result = JOptionPane.showConfirmDialog(null, jpf, msg, JOptionPane.OK_CANCEL_OPTION);
		if (result == 0)
			return new String(jpf.getPassword());
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



