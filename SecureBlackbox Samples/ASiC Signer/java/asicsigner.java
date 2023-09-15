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
import javax.swing.table.DefaultTableModel;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

class asicsigner extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTable tableSourceFiles;
	private JButton btnClear;
	private JButton btnAddInputFile;
	private JTextField edOutputFile;

	private JComboBox<String> cmbLevel;
	private JCheckBox cbExtended;
	private JComboBox<String> cmbSignatureType;

	private JTextField edSignCertFile;
	private JTextField edSignCertPassword;
	private JTable tableCerts;
	private JButton btnAddCerts;
	private JButton btnRemoveCerts;

	private JCheckBox cbTimestamp;
	private JTextField edTimestampServer;

	private JTextField edIdentifier;
	private JComboBox<String> cmbHashAlgorithm;
	private JTextField edHashValue;
	private JTable tableKnown;
	private JButton btnAddKnown;
	private JButton btnRemoveKnown;
	private JTable tableTrusted;
	private JButton btnAddTrusted;
	private JButton btnRemoveTrusted;

	Asicsigner signer;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			asicsigner dialog = new asicsigner();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public asicsigner()
	{
		signer = new Asicsigner();

		setTitle("ASiC Signer demo");
		
		setBounds(100, 100, 880, 700);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample shows how to create ASiC signatures. Please select input files, a signing certificate, and the desired ASiC type and level, and click 'Sign'.");
		lblCaption.setBounds(10, 5, 870, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblInputFile = new JLabel("Input files list (files to sign):");
		lblInputFile.setBounds(10, 25, 180, 14);
		contentPanel.add(lblInputFile);

		JScrollPane scrollPanelInput = new JScrollPane();
		scrollPanelInput.setBounds(10, 45, 340, 90);
		contentPanel.add(scrollPanelInput);

		tableSourceFiles = new JTable();
		tableSourceFiles.setModel(new DefaultTableModel(
				new Object[][] {
				},
				new String[] {
						"Filename"
				}
		));
		tableSourceFiles.setFillsViewportHeight(true);
		scrollPanelInput.setViewportView(tableSourceFiles);

		btnClear = new JButton("Clear");
		btnClear.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				clearClick();
			}
		});
		btnClear.setBounds(355, 45, 80, 25);
		contentPanel.add(btnClear);

		btnAddInputFile = new JButton("Add");
		btnAddInputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addInputFileClick();
			}
		});
		btnAddInputFile.setBounds(355, 80, 80, 25);
		contentPanel.add(btnAddInputFile);

		JLabel lblOutputFile = new JLabel("Output File");
		lblOutputFile.setBounds(10, 143, 61, 14);
		contentPanel.add(lblOutputFile);

		edOutputFile = new JTextField();
		edOutputFile.setBounds(80, 140, 270, 20);
		contentPanel.add(edOutputFile);
		edOutputFile.setColumns(10);

		JButton sbBrowseOutputFile = new JButton("Browse");
		sbBrowseOutputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edOutputFile.setText(getSaveFileName());
			}
		});
		sbBrowseOutputFile.setBounds(355, 138, 80, 25);
		contentPanel.add(sbBrowseOutputFile);

		JPanel Signpane1 = new JPanel();
		Signpane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing options", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Signpane1.setBounds(10, 170, 845, 165);
		contentPanel.add(Signpane1);
		Signpane1.setLayout(null);

		JLabel lblLevel = new JLabel("Level");
		lblLevel.setBounds(10, 23, 40, 14);
		Signpane1.add(lblLevel);

		cmbLevel = new JComboBox<String>();
		cmbLevel.setModel(new DefaultComboBoxModel<String>(new String[] {"BES", "EPES", "T", "C", "XType1", "XType2", "XLType1", "XLType2", "BaselineB", "BaselineT", "BaselineLT", "BaselineLTA", "ExtendedBES", "ExtendedEPES", "ExtendedT", "ExtendedC", "ExtendedXType1", "ExtendedXType2", "ExtendedXLType1", "ExtendedXLType2", "A", "ExtendedA"}));
		cmbLevel.setBounds(55, 20, 140, 22);
		Signpane1.add(cmbLevel);

		cbExtended = new JCheckBox("Extended");
		cbExtended.setBounds(250, 25, 100, 14);
		Signpane1.add(cbExtended);

		JLabel lblSignatureType = new JLabel("Signature type");
		lblSignatureType.setBounds(400, 23, 100, 14);
		Signpane1.add(lblSignatureType);

		cmbSignatureType = new JComboBox<String>();
		cmbSignatureType.setModel(new DefaultComboBoxModel<String>(new String[] {"CAdES", "XAdES"}));
		cmbSignatureType.setBounds(500, 20, 110, 22);
		Signpane1.add(cmbSignatureType);

		JLabel lblSignCertFile = new JLabel("Signing certificate:");
		lblSignCertFile.setBounds(10, 55, 150, 14);
		Signpane1.add(lblSignCertFile);

		edSignCertFile = new JTextField();
		edSignCertFile.setBounds(10, 75, 310, 20);
		edSignCertFile.setColumns(10);
		Signpane1.add(edSignCertFile);

		JButton sbBrowseSignCertFile = new JButton("Browse");
		sbBrowseSignCertFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edSignCertFile.setText(getSaveFileName());
			}
		});
		sbBrowseSignCertFile.setBounds(330, 73, 80, 25);
		Signpane1.add(sbBrowseSignCertFile);

		JLabel lblSignCertPassword = new JLabel("Certificate password:");
		lblSignCertPassword.setBounds(10, 108, 130, 14);
		Signpane1.add(lblSignCertPassword);

		edSignCertPassword = new JTextField();
		edSignCertPassword.setBounds(140, 105, 270, 20);
		edSignCertPassword.setColumns(10);
		Signpane1.add(edSignCertPassword);

		JPanel pane1 = new JPanel();
		pane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing Chain", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane1.setBounds(430, 50, 410, 110);
		Signpane1.add(pane1);
		pane1.setLayout(null);

		JScrollPane scrollPanelChain = new JScrollPane();
		scrollPanelChain.setBounds(5, 20, 310, 85);
		pane1.add(scrollPanelChain);

		tableCerts = new JTable();
		tableCerts.setModel(new DefaultTableModel(
				new Object[][] {
				},
				new String[] {
						"Serial", "Issuer"
				}
		));
		tableCerts.setFillsViewportHeight(true);
		scrollPanelChain.setViewportView(tableCerts);

		btnAddCerts = new JButton("Add");
		btnAddCerts.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addCertsClick();
			}
		});
		btnAddCerts.setBounds(320, 20, 80, 25);
		pane1.add(btnAddCerts);

		btnRemoveCerts = new JButton("Remove");
		btnRemoveCerts.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeCertsClick();
			}
		});
		btnRemoveCerts.setBounds(320, 55, 80, 25);
		pane1.add(btnRemoveCerts);

		JPanel Additianalpane1 = new JPanel();
		Additianalpane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Additional options", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Additianalpane1.setBounds(10, 340, 845, 140);
		contentPanel.add(Additianalpane1);
		Additianalpane1.setLayout(null);

		JPanel pane11 = new JPanel();
		pane11.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Policy", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane11.setBounds(5, 20, 410, 110);
		Additianalpane1.add(pane11);
		pane11.setLayout(null);

		JLabel lblIdentifier = new JLabel("Identifier:");
		lblIdentifier.setBounds(10, 23, 90, 14);
		pane11.add(lblIdentifier);

		edIdentifier = new JTextField();
		edIdentifier.setBounds(110, 20, 290, 20);
		pane11.add(edIdentifier);
		edIdentifier.setColumns(10);

		JLabel lblHashAlgorithm = new JLabel("Hash algorithm:");
		lblHashAlgorithm.setBounds(10, 53, 90, 14);
		pane11.add(lblHashAlgorithm);

		cmbHashAlgorithm = new JComboBox<String>();
		cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"", "SHA1", "MD5", "SHA256", "SHA384", "SHA512", "RIPEMD160"}));
		cmbHashAlgorithm.setBounds(110, 50, 140, 22);
		pane11.add(cmbHashAlgorithm);

		JLabel lblHashValue = new JLabel("Hash value:");
		lblHashValue.setBounds(10, 83, 90, 14);
		pane11.add(lblHashValue);

		edHashValue = new JTextField();
		edHashValue.setBounds(110, 80, 290, 20);
		pane11.add(edHashValue);
		edHashValue.setColumns(10);

		cbTimestamp = new JCheckBox("Request a timestamp from TSA server:");
		cbTimestamp.setBounds(430, 80, 250, 23);
		Additianalpane1.add(cbTimestamp);

		edTimestampServer = new JTextField();
		edTimestampServer.setText("http://");
		edTimestampServer.setBounds(445, 108, 390, 20);
		edTimestampServer.setColumns(10);
		Additianalpane1.add(edTimestampServer);

		JPanel RevInfopanel = new JPanel();
		RevInfopanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Revocation information", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		RevInfopanel.setBounds(10, 485, 845, 135);
		contentPanel.add(RevInfopanel);
		RevInfopanel.setLayout(null);

		JPanel Knownpanel = new JPanel();
		Knownpanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Knownpanel.setBounds(5, 20, 410, 110);
		RevInfopanel.add(Knownpanel);
		Knownpanel.setLayout(null);

		JScrollPane scrollPane2 = new JScrollPane();
		scrollPane2.setBounds(5, 20, 310, 85);
		Knownpanel.add(scrollPane2);

		tableKnown = new JTable();
		tableKnown.setModel(new DefaultTableModel(
				new Object[][] {
				},
				new String[] {
						"Serial", "Issuer"
				}
		));
		tableKnown.setFillsViewportHeight(true);
		scrollPane2.setViewportView(tableKnown);

		btnAddKnown = new JButton("Add");
		btnAddKnown.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addKnownClick();
			}
		});
		btnAddKnown.setBounds(320, 20, 80, 25);
		Knownpanel.add(btnAddKnown);

		btnRemoveKnown = new JButton("Remove");
		btnRemoveKnown.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeKnownClick();
			}
		});
		btnRemoveKnown.setBounds(320, 55, 80, 25);
		Knownpanel.add(btnRemoveKnown);

		JPanel Trustpanel = new JPanel();
		Trustpanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Trusted Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Trustpanel.setBounds(430, 20, 410, 110);
		RevInfopanel.add(Trustpanel);
		Trustpanel.setLayout(null);

		JScrollPane scrollPane3 = new JScrollPane();
		scrollPane3.setBounds(5, 20, 310, 85);
		Trustpanel.add(scrollPane3);

		tableTrusted = new JTable();
		tableTrusted.setModel(new DefaultTableModel(
				new Object[][] {
				},
				new String[] {
						"Serial", "Issuer"
				}
		));
		tableTrusted.setFillsViewportHeight(true);
		scrollPane3.setViewportView(tableTrusted);

		btnAddTrusted = new JButton("Add");
		btnAddTrusted.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addTrustedClick();
			}
		});
		btnAddTrusted.setBounds(320, 20, 80, 25);
		Trustpanel.add(btnAddTrusted);

		btnRemoveTrusted = new JButton("Remove");
		btnRemoveTrusted.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeTrustedClick();
			}
		});
		btnRemoveTrusted.setBounds(320, 55, 80, 25);
		Trustpanel.add(btnRemoveTrusted);

		JButton btnSign = new JButton("Sign");
		btnSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sign();
			}
		});
		btnSign.setBounds(770, 630, 80, 25);
		contentPanel.add(btnSign);
	}

	protected void addInputFileClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			DefaultTableModel model = (DefaultTableModel)tableSourceFiles.getModel();
			model.insertRow(model.getRowCount(), new Object[] {fileName});
		}
	}

	protected void clearClick()
	{
		((DefaultTableModel)tableSourceFiles.getModel()).setNumRows(0);
	}

	private String requestPassword() {
		JPasswordField jpf = new JPasswordField();
		int result = JOptionPane.showConfirmDialog(null, jpf, "Please enter password", JOptionPane.OK_CANCEL_OPTION);
		if (result == 0)
			return new String(jpf.getPassword());
		return "";
	}

	protected void addCertsClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			signer.getSigningChain().add(cert);
			updateSignCertificates();
		}
	}

	protected void removeCertsClick()
	{
		signer.getSigningChain().remove(tableCerts.getSelectedRow());
		((DefaultTableModel)tableCerts.getModel()).removeRow(tableCerts.getSelectedRow());
	}

	private void addSignCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)tableCerts.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearSignCertificatesTable() {
		((DefaultTableModel)tableCerts.getModel()).setNumRows(0);
	}

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	private void updateSignCertificates() {
		clearSignCertificatesTable();

		for (int i = 0; i < signer.getSigningChain().size(); i++)
		{
			String s = signer.getSigningChain().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addSignCertItem(bytesToHex(signer.getSigningChain().item(i).getSerialNumber()), s);
		}
	}

	protected void addKnownClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			signer.getKnownCertificates().add(cert);
			updateKnownCertificates();
		}
	}

	protected void removeKnownClick()
	{
		signer.getKnownCertificates().remove(tableKnown.getSelectedRow());
		((DefaultTableModel)tableKnown.getModel()).removeRow(tableKnown.getSelectedRow());
	}

	private void addKnownCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)tableKnown.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearKnownCertificatesTable() {
		((DefaultTableModel)tableKnown.getModel()).setNumRows(0);
	}

	private void updateKnownCertificates() {
		clearKnownCertificatesTable();

		for (int i = 0; i < signer.getKnownCertificates().size(); i++)
		{
			String s = signer.getKnownCertificates().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addKnownCertItem(bytesToHex(signer.getKnownCertificates().item(i).getSerialNumber()), s);
		}
	}

	protected void addTrustedClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			signer.getTrustedCertificates().add(cert);
			updateTrustedCertificates();
		}
	}

	protected void removeTrustedClick()
	{
		signer.getTrustedCertificates().remove(tableTrusted.getSelectedRow());
		((DefaultTableModel)tableTrusted.getModel()).removeRow(tableTrusted.getSelectedRow());
	}

	private void addTrustedCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)tableTrusted.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearTrustedCertificatesTable() {
		((DefaultTableModel)tableTrusted.getModel()).setNumRows(0);
	}

	private void updateTrustedCertificates() {
		clearTrustedCertificatesTable();

		for (int i = 0; i < signer.getTrustedCertificates().size(); i++)
		{
			String s = signer.getTrustedCertificates().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addTrustedCertItem(bytesToHex(signer.getTrustedCertificates().item(i).getSerialNumber()), s);
		}
	}

	protected void sign()
	{
		try
		{
			String SourceFiles = "";
			if (tableSourceFiles.getRowCount() > 0)
			{
				SourceFiles = tableSourceFiles.getModel().getValueAt(0, 0).toString();
				for (int i = 1; i < tableSourceFiles.getRowCount(); i++)
				{
					SourceFiles = SourceFiles + "\r\n" + tableSourceFiles.getModel().getValueAt(i, 0).toString();
				}
			}
			signer.setSourceFiles(SourceFiles);
			signer.setOutputFile(edOutputFile.getText());

			switch (cmbLevel.getSelectedIndex())
			{
				case 0: signer.getNewSignature().setLevel(ASiCSignature.aslBES); break;
				case 1: signer.getNewSignature().setLevel(ASiCSignature.aslEPES); break;
				case 2: signer.getNewSignature().setLevel(ASiCSignature.aslT); break;
				case 3: signer.getNewSignature().setLevel(ASiCSignature.aslC); break;
				case 4: signer.getNewSignature().setLevel(ASiCSignature.aslXType1); break;
				case 5: signer.getNewSignature().setLevel(ASiCSignature.aslXType2); break;
				case 6: signer.getNewSignature().setLevel(ASiCSignature.aslXLType1); break;
				case 7: signer.getNewSignature().setLevel(ASiCSignature.aslXLType2); break;
				case 8: signer.getNewSignature().setLevel(ASiCSignature.aslBaselineB); break;
				case 9: signer.getNewSignature().setLevel(ASiCSignature.aslBaselineT); break;
				case 10: signer.getNewSignature().setLevel(ASiCSignature.aslBaselineLT); break;
				case 11: signer.getNewSignature().setLevel(ASiCSignature.aslBaselineLTA); break;
				case 12: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedBES); break;
				case 13: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedEPES); break;
				case 14: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedT); break;
				case 15: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedC); break;
				case 16: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedXType1); break;
				case 17: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedXType2); break;
				case 18: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedXLType1); break;
				case 19: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedXLType2); break;
				case 20: signer.getNewSignature().setLevel(ASiCSignature.aslA); break;
				case 21: signer.getNewSignature().setLevel(ASiCSignature.aslExtendedA); break;
			}

			signer.setExtended(cbExtended.isSelected());

			switch (cmbSignatureType.getSelectedIndex())
			{
				case 0: signer.getNewSignature().setSignatureType(ASiCSignature.castCAdES); break;
				case 1: signer.getNewSignature().setSignatureType(ASiCSignature.castXAdES); break;
			}

			signer.setSigningCertificate(LoadCertificate(edSignCertFile.getText(), edSignCertPassword.getText()));

			if (cbTimestamp.isSelected())
			{
				signer.setTimestampServer(edTimestampServer.getText());
			}
			else
			{
				signer.setTimestampServer("");
			}

			signer.getNewSignature().setPolicyID (edIdentifier.getText());
			signer.getNewSignature().setPolicyHashAlgorithm(cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));
			signer.getNewSignature().setPolicyHash(edHashValue.getText());

			signer.setIgnoreChainValidationErrors(true);

			signer.sign();

			showMessage("Info", "The file(s) successfully signed");
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



