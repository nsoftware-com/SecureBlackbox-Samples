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
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import secureblackbox.*;

public class pgpwriter extends JDialog {

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
	private Pgpwriter writer;
	private Pgpkeyring keyring;

	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;
	private JComboBox<ItemKey> encryptKeys;
	private JComboBox<ItemKey> signKeys;
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					pgpwriter dialog = new pgpwriter();
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
	public pgpwriter() {
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				close();
			}
		});

		setResizable(false);
		setTitle("PGPWriter Demo Application");

		setBounds(100, 100, 700, 320);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample showcases the use of PGPWriter to create encrypted and signed OpenPGP-compliant files. ");
		lblCaption.setBounds(10, 5, 690, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblInputFile = new JLabel("File to protect");
		lblInputFile.setBounds(10, 33, 90, 14);
		contentPanel.add(lblInputFile);

		edInputFile = new JTextField();
		edInputFile.setBounds(100, 30, 440, 20);
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
		panelOptions.setBounds(5, 90, 685, 160);
		contentPanel.add(panelOptions);
		panelOptions.setLayout(null);

		JButton btnBrowseKeyring= new JButton("Browse keyring files...");
		btnBrowseKeyring.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				LoadKeyring();
			}
		});
		btnBrowseKeyring.setBounds(510, 25, 160, 25);
		panelOptions.add(btnBrowseKeyring);

		JLabel lblDecryptKeys = new JLabel("Please select the encryption key:");
		lblDecryptKeys.setBounds(10, 75, 200, 14);
		panelOptions.add(lblDecryptKeys);

		encryptKeys = new JComboBox<ItemKey>();
		encryptKeys.setBounds(210, 73, 460, 23);
		panelOptions.add(encryptKeys);

		JLabel lblVerifyKeys = new JLabel("Please select the signing key:");
		lblVerifyKeys.setBounds(10, 115, 200, 14);
		panelOptions.add(lblVerifyKeys);

		signKeys = new JComboBox<ItemKey>();
		signKeys.setBounds(210, 113, 460, 23);
		panelOptions.add(signKeys);

		JPanel buttonPane = new JPanel();
		buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
		getContentPane().add(buttonPane, BorderLayout.SOUTH);

		JButton okButton = new JButton("Encrypt and sign");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				EncryptAndSign();
			}
		});
		okButton.setActionCommand("Encrypt and sign");
		buttonPane.add(okButton);
		getRootPane().setDefaultButton(okButton);

		init();
	}

	protected void close()
	{
		try
		{
			writer.dispose();
			keyring.dispose();
		}
		catch (Exception e)
		{}
		dispose();
		System.exit(0);
	}

	private void init() {
		writer = new Pgpwriter();
		keyring = new Pgpkeyring();
		try
		{
			writer.addPgpwriterEventListener(new PgpwriterEventListener() {
				@Override
				public void error(PgpwriterErrorEvent e) {

				}

				@Override
				public void externalSign(PgpwriterExternalSignEvent e) {

				}

				@Override
				public void keyPassphraseNeeded(PgpwriterKeyPassphraseNeededEvent e) {
					e.passphrase = requestPassword(GetDefaultUserID(e.userID, e.keyID));
					e.skip = (e.passphrase == "");
				}

				@Override
				public void notification(PgpwriterNotificationEvent e) {

				}

				@Override
				public void progress(PgpwriterProgressEvent e) {

				}

				@Override
				public void supercoreIntercept(PgpwriterSupercoreInterceptEvent e) {

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
		encryptKeys.removeAllItems();
		signKeys.removeAllItems();

		encryptKeys.addItem(new ItemKey("", null));
		for (int i = 0; i < keyring.getPublicKeys().size(); i++)
		{
			encryptKeys.addItem(new ItemKey(GetDefaultUserID(keyring.getPublicKeys().item(i)), keyring.getPublicKeys().item(i)));
		}

		signKeys.addItem(new ItemKey("", null));
		for (int i = 0; i < keyring.getSecretKeys().size(); i++)
		{
			signKeys.addItem(new ItemKey(GetDefaultUserID(keyring.getSecretKeys().item(i)), keyring.getSecretKeys().item(i)));
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

	private void EncryptAndSign()
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
		if (encryptKeys.getSelectedIndex() <= 0) {
			showErrorMessage("Please select the encryption key", "Error");
		}
		else
		if (signKeys.getSelectedIndex() <= 0) {
			showErrorMessage("Please select the signing key", "Error");
		}
		else
		{
			try {
				writer.setArmor(true);
				writer.setArmorHeaders("Version: OpenPGPBlackbox");
				writer.setArmorBoundary("PGP MESSAGE");
				writer.setInputIsText(false);
				writer.setProtection(Pgpwriter.pptNormal);
				writer.setEncryptionAlgorithm("3DES");
				writer.getEncryptingKeys().clear();
				writer.getEncryptingKeys().add(((ItemKey)encryptKeys.getSelectedItem()).Value);
				writer.getSigningKeys().clear();
				writer.getSigningKeys().add(((ItemKey)signKeys.getSelectedItem()).Value);
				writer.setTimestamp(new SimpleDateFormat("dd-MM-yyyy hh:mm:ss").format(new Date()));
				writer.setFilename(fileName);

				writer.encryptAndSignFile(edInputFile.getText(), edOutputFile.getText());
				showMessage("The file was encrypted successfully", "Information");
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage(), "Error");
			}
			close();
		}
	}

	private void showErrorMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.ERROR_MESSAGE);				
	}

	private void showMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.INFORMATION_MESSAGE);
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



