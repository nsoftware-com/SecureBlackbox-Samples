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
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.event.*;
import java.io.File;
import java.util.TooManyListenersException;

import secureblackbox.*;

public class kmipclient extends JDialog {
	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JButton btnRefresh;
	private JButton btnSettings;
	private JButton btnCreateKey;
	private JButton btnCreateCert;
	private JButton btnAddKey;
	private JButton btnAddCert;
	private JButton btnRemove;
	private JButton btnEncrypt;
	private JButton btnDecrypt;
	private JButton btnSign;
	private JButton btnVerify;
	private JTable tableObjects;
	private Kmipclient client;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try
		{
			kmipclient dialog = new kmipclient();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public kmipclient() {
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				System.exit(0);
			}
		});
		setResizable(false);
		setTitle("KMIP Client demo application");
		setBounds(100, 100, 1100, 600);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates basic KMIP client operations.");
		lblCaption.setBounds(10, 5, 690, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		btnRefresh = new JButton("Refresh");
		btnRefresh.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				refreshObjects();
			}
		});
		btnRefresh.setFont(new Font("Arial", Font.BOLD, 11));
		btnRefresh.setBounds(10, 30, 130, 25);
		contentPanel.add(btnRefresh);

		btnSettings = new JButton("Settings");
		btnSettings.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showSettings();
			}
		});
		btnSettings.setFont(new Font("Arial", Font.BOLD, 11));
		btnSettings.setBounds(150, 30, 130, 25);
		contentPanel.add(btnSettings);

		btnCreateKey = new JButton("Create key");
		btnCreateKey.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				createKey();
			}
		});
		btnCreateKey.setFont(new Font("Arial", Font.BOLD, 11));
		btnCreateKey.setBounds(350, 30, 130, 25);
		contentPanel.add(btnCreateKey);

		btnCreateCert = new JButton("Create certificate");
		btnCreateCert.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				createCertificate();
			}
		});
		btnCreateCert.setFont(new Font("Arial", Font.BOLD, 11));
		btnCreateCert.setBounds(490, 30, 130, 25);
		contentPanel.add(btnCreateCert);

		btnAddKey = new JButton("Add key");
		btnAddKey.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addKey();
			}
		});
		btnAddKey.setFont(new Font("Arial", Font.BOLD, 11));
		btnAddKey.setBounds(630, 30, 130, 25);
		contentPanel.add(btnAddKey);

		btnAddCert = new JButton("Add certificate");
		btnAddCert.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addCertificate();
			}
		});
		btnAddCert.setFont(new Font("Arial", Font.BOLD, 11));
		btnAddCert.setBounds(770, 30, 130, 25);
		contentPanel.add(btnAddCert);

		btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				remove();
			}
		});
		btnRemove.setFont(new Font("Arial", Font.BOLD, 11));
		btnRemove.setBounds(910, 30, 130, 25);
		contentPanel.add(btnRemove);


		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 70, 950, 500);
		contentPanel.add(scrollPane);
		{
			tableObjects = new JTable(new objectstablemodel());
			tableObjects.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				@Override
				public void valueChanged(ListSelectionEvent e) {
					updateControls();
				}
			});
			tableObjects.setFillsViewportHeight(true);
			scrollPane.setViewportView(tableObjects);
		}


		btnEncrypt = new JButton("Encrypt");
		btnEncrypt.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (tableObjects.getSelectedRowCount() > 0)
				{
					objectstablemodel model = (objectstablemodel) tableObjects.getModel();
					String UniqueIdentifier = model.getSelectedRowTag(tableObjects.getSelectedRow()).getUniqueIdentifier();

					operationdialog frm = new operationdialog(client, UniqueIdentifier, operationdialog.Op_Encrypt);
					frm.setModal(true);
					frm.setVisible(true);
					frm.dispose();
				}
			}
		});
		btnEncrypt.setFont(new Font("Arial", Font.BOLD, 11));
		btnEncrypt.setBounds(960, 70, 130, 25);
		contentPanel.add(btnEncrypt);

		btnDecrypt = new JButton("Decrypt");
		btnDecrypt.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (tableObjects.getSelectedRowCount() > 0)
				{
					objectstablemodel model = (objectstablemodel) tableObjects.getModel();
					String UniqueIdentifier = model.getSelectedRowTag(tableObjects.getSelectedRow()).getUniqueIdentifier();

					operationdialog frm = new operationdialog(client, UniqueIdentifier, operationdialog.Op_Decrypt);
					frm.setModal(true);
					frm.setVisible(true);
					frm.dispose();
				}
			}
		});
		btnDecrypt.setFont(new Font("Arial", Font.BOLD, 11));
		btnDecrypt.setBounds(960, 105, 130, 25);
		contentPanel.add(btnDecrypt);

		btnSign = new JButton("Sign");
		btnSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (tableObjects.getSelectedRowCount() > 0)
				{
					objectstablemodel model = (objectstablemodel) tableObjects.getModel();
					String UniqueIdentifier = model.getSelectedRowTag(tableObjects.getSelectedRow()).getUniqueIdentifier();

					operationdialog frm = new operationdialog(client, UniqueIdentifier, operationdialog.Op_Sign);
					frm.setModal(true);
					frm.setVisible(true);
					frm.dispose();
				}
			}
		});
		btnSign.setFont(new Font("Arial", Font.BOLD, 11));
		btnSign.setBounds(960, 140, 130, 25);
		contentPanel.add(btnSign);

		btnVerify = new JButton("Verify");
		btnVerify.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (tableObjects.getSelectedRowCount() > 0)
				{
					objectstablemodel model = (objectstablemodel) tableObjects.getModel();
					String UniqueIdentifier = model.getSelectedRowTag(tableObjects.getSelectedRow()).getUniqueIdentifier();

					operationdialog frm = new operationdialog(client, UniqueIdentifier, operationdialog.Op_Verify);
					frm.setModal(true);
					frm.setVisible(true);
					frm.dispose();
				}
			}
		});
		btnVerify.setFont(new Font("Arial", Font.BOLD, 11));
		btnVerify.setBounds(960, 175, 130, 25);
		contentPanel.add(btnVerify);


		init();
	}

	private void init()
	{
		client = new Kmipclient();
		try
		{
			client.config("BlockSize=16000");
		}
		catch (Exception ex)
		{}

		updateControls();
	}

	private void updateControls()
	{
		objectstablemodel model = (objectstablemodel) tableObjects.getModel();

		btnRemove.setEnabled(tableObjects.getSelectedRowCount() > 0);

		if (tableObjects.getSelectedRowCount() > 0)
		{
			btnEncrypt.setEnabled((model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otCertificate) ||
					(model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otSymmetricKey) ||
					((model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otPublicKey) && model.getSelectedRowTag(tableObjects.getSelectedRow()).getKeyAlgorithm().contains("RSA"))
			);

			btnDecrypt.setEnabled((model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otCertificate) ||
					(model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otSymmetricKey) ||
					((model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otPrivateKey) && model.getSelectedRowTag(tableObjects.getSelectedRow()).getKeyAlgorithm().contains("RSA"))
			);

			btnSign.setEnabled((model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otCertificate) ||
					(model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otPrivateKey)
			);

			btnVerify.setEnabled((model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otCertificate) ||
					(model.getSelectedRowTag(tableObjects.getSelectedRow()).getObjectType() == KMIPObject.otPublicKey)
			);
		}
		else
		{
			btnEncrypt.setEnabled(false);
			btnDecrypt.setEnabled(false);
			btnSign.setEnabled(false);
			btnVerify.setEnabled(false);
		}
	}

	private void showSettings()
	{
		conndialog frm = new conndialog();
		frm.setModal(true);
		frm.setVisible(true);

		if (frm.isOK())
		{
			try
			{
				client.setHost(frm.getHost());
				client.setPort(frm.getPort());

				client.setEncoderType(frm.getEncoderType());

				client.setUsername(frm.getUsername());
				client.setPassword(frm.getPassword());

				refreshObjects();
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}

		frm.dispose();
	}

	private void refreshObjects()
	{
		try
		{
			clearObjectList();

			client.getList(KMIPObject.otUnknown);

			objectstablemodel model = (objectstablemodel) tableObjects.getModel();

			for(int i = 0; i < client.getObjects().size(); i++)
			{
				model.addItem(client.getObjects().get(i));
			}
		}
		catch (Exception ex)
		{
			showErrorMessage(ex.getMessage());
		}
	}

	private void createKey()
	{
		createkeydialog frm = new createkeydialog();
		frm.setModal(true);
		frm.setVisible(true);

		if (frm.isOK())
		{
			try
			{
				client.config("Curve=" + frm.getCurve());

				client.generateKey(frm.getPublicAlgorithm(), frm.getKeyLength(), frm.getId());

				refreshObjects();
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}

		frm.dispose();
	}

	private void createCertificate()
	{
		createcertdialog frm = new createcertdialog();
		frm.setModal(true);
		frm.setVisible(true);

		if (frm.isOK())
		{
			try
			{
				client.config("Curve=" + frm.getCurve());

				client.generateCert("", frm.getPublicAlgorithm(), frm.getHashAlgorithm(),
						frm.getKeyLength(), frm.getSubject(), frm.getId());

				refreshObjects();
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}

		frm.dispose();
	}

	private void addKey()
	{
		addkeydialog frm = new addkeydialog();
		frm.setModal(true);
		frm.setVisible(true);

		if (frm.isOK())
		{
			try
			{
				client.setInputFile(frm.getKeyFile());
				client.config("Curve=" + frm.getCurve());

				client.addKey(frm.getId());

				refreshObjects();
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}

		frm.dispose();
	}

	private void addCertificate()
	{
		addcertdialog frm = new addcertdialog();
		frm.setModal(true);
		frm.setVisible(true);

		if (frm.isOK())
		{
			try
			{
				client.setInputFile(frm.getCertFile());

				client.addCertificate(frm.getPassword(), true, frm.getId());

				refreshObjects();
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage());
			}
		}

		frm.dispose();
	}

	private void remove()
	{
		if (tableObjects.getSelectedRowCount() > 0)
		{
			objectstablemodel model = (objectstablemodel) tableObjects.getModel();
			String UniqueIdentifier = model.getSelectedRowTag(tableObjects.getSelectedRow()).getUniqueIdentifier();
			String ObjType = model.getSelectedRowObjectType(tableObjects.getSelectedRow());

			if (showConfirmMessage("Remove " + ObjType + " \"" + UniqueIdentifier + "\"?", "Delete object")) {
				try
				{
					client.remove(UniqueIdentifier);

					refreshObjects();
				}
				catch (Exception ex)
				{
					showErrorMessage(ex.getMessage());
				}
			}
		}
	}

	private boolean showConfirmMessage(String msg, String caption) {
		return (JOptionPane.showConfirmDialog(this, msg, caption, JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION);
	}

	static void showMessage(String msg, String caption) {
		JOptionPane.showMessageDialog(null, msg, caption, JOptionPane.INFORMATION_MESSAGE);
	}

	static void showErrorMessage(String msg) {
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	private void clearObjectList()
	{
		((objectstablemodel)tableObjects.getModel()).clear();
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



