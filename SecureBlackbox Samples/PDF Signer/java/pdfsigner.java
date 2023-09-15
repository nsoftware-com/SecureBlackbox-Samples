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

class pdfsigner extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;

	private JComboBox<String> cmbLevel;
	private JCheckBox cbVisible;
	private JTextField edSignCertFile;
	private JTextField edSignCertPassword;
	private JTable tableSign;
	private JButton btnAddSign;
	private JButton btnRemoveSign;

	private JCheckBox cbAutoCollectRevInfo;
	private JCheckBox cbIgnoreChainValidationErrors;
	private JCheckBox cbForceCompleteChainValidation;
	private JCheckBox cbDeepValidation;
	private JTextField edIdentifier;
	private JComboBox<String> cmbHashAlgorithm;
	private JTextField edHashValue;
	private JTextField edAuthor;
	private JTextField edReason;
	private JCheckBox cbTimestamp;
	private JTextField edTimestampServer;

	private JTable tableKnown;
	private JButton btnAddKnown;
	private JButton btnRemoveKnown;
	private JTable tableTrusted;
	private JButton btnAddTrusted;
	private JButton btnRemoveTrusted;
	private JCheckBox cbIncludeLocalRevInfo;
	private JCheckBox cbIncludeRevocationInfoToAdbeAttribute;

	Pdfsigner signer;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			pdfsigner dialog = new pdfsigner();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public pdfsigner()
	{
		signer = new Pdfsigner();

		setTitle("PDF Signer");
		
		setBounds(100, 100, 800, 675);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates the use of PDFSigner component for signing PDF documents. Please pick the signing certificate and click 'Sign'.");
		lblCaption.setBounds(10, 5, 790, 14);
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
		panelOptions.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing options  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panelOptions.setBounds(5, 90, 775, 165);
		contentPanel.add(panelOptions);
		panelOptions.setLayout(null);

		JLabel lblLevel = new JLabel("Level");
		lblLevel.setBounds(10, 28, 53, 14);
		panelOptions.add(lblLevel);

		cmbLevel = new JComboBox<String>();
		cmbLevel.setModel(new DefaultComboBoxModel<String>(new String[] {"Legacy", "BES", "EPES", "LTV", "DocumentTimestamp"}));
		cmbLevel.setBounds(50, 25, 100, 22);
		panelOptions.add(cmbLevel);

		cbVisible = new JCheckBox("Visible signature");
		cbVisible.setBounds(180, 25, 150, 23);
		panelOptions.add(cbVisible);

		JLabel lblSignCertFile = new JLabel("Signing certificate:");
		lblSignCertFile.setBounds(10, 63, 150, 14);
		panelOptions.add(lblSignCertFile);

		edSignCertFile = new JTextField();
		edSignCertFile.setBounds(10, 80, 280, 20);
		panelOptions.add(edSignCertFile);
		edSignCertFile.setColumns(10);

		JButton sbBrowseSignCertFile = new JButton("Browse");
		sbBrowseSignCertFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edSignCertFile.setText(getSaveFileName());
			}
		});
		sbBrowseSignCertFile.setBounds(300, 78, 80, 25);
		panelOptions.add(sbBrowseSignCertFile);

		JLabel lblSignCertPassword = new JLabel("Certificate password:");
		lblSignCertPassword.setBounds(10, 113, 130, 14);
		panelOptions.add(lblSignCertPassword);

		edSignCertPassword = new JTextField();
		edSignCertPassword.setBounds(140, 110, 240, 20);
		panelOptions.add(edSignCertPassword);
		edSignCertPassword.setColumns(10);

		JPanel panel_2 = new JPanel();
		panel_2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing Chain", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_2.setBounds(395, 60, 375, 100);
		panelOptions.add(panel_2);
		panel_2.setLayout(null);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 20, 270, 75);
		panel_2.add(scrollPane);

		tableSign = new JTable();
		tableSign.setModel(new DefaultTableModel(
				new Object[][] {
				},
				new String[] {
						"Serial", "Issuer"
				}
		));
		tableSign.setFillsViewportHeight(true);
		scrollPane.setViewportView(tableSign);

		btnAddSign = new JButton("Add");
		btnAddSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addSignClick();
			}
		});
		btnAddSign.setBounds(285, 20, 80, 25);
		panel_2.add(btnAddSign);

		btnRemoveSign = new JButton("Remove");
		btnRemoveSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeSignClick();
			}
		});
		btnRemoveSign.setBounds(285, 55, 80, 25);
		panel_2.add(btnRemoveSign);

		JPanel panelAddOptions = new JPanel();
		panelAddOptions.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Additional options  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panelAddOptions.setBounds(5, 255, 775, 180);
		contentPanel.add(panelAddOptions);
		panelAddOptions.setLayout(null);

		JLabel lblAuthor = new JLabel("Author's name:");
		lblAuthor.setBounds(10, 28, 90, 14);
		panelAddOptions.add(lblAuthor);

		edAuthor = new JTextField();
		edAuthor.setText("");
		edAuthor.setBounds(105, 25, 245, 20);
		panelAddOptions.add(edAuthor);
		edAuthor.setColumns(10);

		JLabel lblReason = new JLabel("Reason for signing:");
		lblReason.setBounds(10, 58, 110, 14);
		panelAddOptions.add(lblReason);

		edReason = new JTextField();
		edReason.setText("");
		edReason.setBounds(125, 55, 225, 20);
		panelAddOptions.add(edReason);
		edReason.setColumns(10);

		cbAutoCollectRevInfo = new JCheckBox("Automatically collect missing revocation information");
		cbAutoCollectRevInfo.setBounds(5, 90, 350, 23);
		panelAddOptions.add(cbAutoCollectRevInfo);

		cbIgnoreChainValidationErrors = new JCheckBox("Ignore chain validation errors");
		cbIgnoreChainValidationErrors.setBounds(5, 115, 350, 23);
		panelAddOptions.add(cbIgnoreChainValidationErrors);

		cbForceCompleteChainValidation = new JCheckBox("Force complete chain validation");
		cbForceCompleteChainValidation.setBounds(5, 140, 210, 23);
		panelAddOptions.add(cbForceCompleteChainValidation);

		cbDeepValidation = new JCheckBox("Deep validation");
		cbDeepValidation.setBounds(220, 140, 120, 23);
		panelAddOptions.add(cbDeepValidation);

		JPanel pane1 = new JPanel();
		pane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Policy", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane1.setBounds(395, 15, 375, 110);
		panelAddOptions.add(pane1);
		pane1.setLayout(null);

		JLabel lblIdentifier = new JLabel("Identifier:");
		lblIdentifier.setBounds(10, 23, 90, 14);
		pane1.add(lblIdentifier);

		edIdentifier = new JTextField();
		edIdentifier.setBounds(110, 20, 220, 20);
		pane1.add(edIdentifier);
		edIdentifier.setColumns(10);

		JLabel lblHashAlgorithm = new JLabel("Hash algorithm:");
		lblHashAlgorithm.setBounds(10, 53, 90, 14);
		pane1.add(lblHashAlgorithm);

		cmbHashAlgorithm = new JComboBox<String>();
		cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"", "SHA1", "MD5", "SHA256", "SHA384", "SHA512", "RIPEMD160"}));
		cmbHashAlgorithm.setBounds(110, 50, 160, 22);
		pane1.add(cmbHashAlgorithm);

		JLabel lblHashValue = new JLabel("Hash value:");
		lblHashValue.setBounds(10, 83, 90, 14);
		pane1.add(lblHashValue);

		edHashValue = new JTextField();
		edHashValue.setBounds(110, 80, 220, 20);
		pane1.add(edHashValue);
		edHashValue.setColumns(10);

		cbTimestamp = new JCheckBox("Request a timestamp from TSA server:");
		cbTimestamp.setBounds(393, 125, 250, 23);
		panelAddOptions.add(cbTimestamp);

		edTimestampServer = new JTextField();
		edTimestampServer.setText("http://");
		edTimestampServer.setBounds(405, 150, 270, 20);
		panelAddOptions.add(edTimestampServer);
		edTimestampServer.setColumns(10);

		JPanel pane2 = new JPanel();
		pane2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Revocation information", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane2.setBounds(5, 435, 775, 160);
		contentPanel.add(pane2);
		pane2.setLayout(null);

		cbIncludeLocalRevInfo = new JCheckBox("Include local revocation information to the signature");
		cbIncludeLocalRevInfo.setBounds(10, 25, 350, 23);
		pane2.add(cbIncludeLocalRevInfo);

		cbIncludeRevocationInfoToAdbeAttribute = new JCheckBox("Include revocation information to Adbe attribute");
		cbIncludeRevocationInfoToAdbeAttribute.setBounds(380, 25, 350, 23);
		pane2.add(cbIncludeRevocationInfoToAdbeAttribute);

		JPanel pane2_1 = new JPanel();
		pane2_1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane2_1.setBounds(5, 55, 375, 100);
		pane2.add(pane2_1);
		pane2_1.setLayout(null);

		JScrollPane scrollPane2 = new JScrollPane();
		scrollPane2.setBounds(5, 20, 270, 75);
		pane2_1.add(scrollPane2);

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
		btnAddKnown.setBounds(285, 20, 80, 25);
		pane2_1.add(btnAddKnown);

		btnRemoveKnown = new JButton("Remove");
		btnRemoveKnown.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeKnownClick();
			}
		});
		btnRemoveKnown.setBounds(285, 55, 80, 25);
		pane2_1.add(btnRemoveKnown);

		JPanel pane2_2 = new JPanel();
		pane2_2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Trusted Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane2_2.setBounds(395, 55, 375, 100);
		pane2.add(pane2_2);
		pane2_2.setLayout(null);

		JScrollPane scrollPane3 = new JScrollPane();
		scrollPane3.setBounds(5, 20, 270, 75);
		pane2_2.add(scrollPane3);

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
		btnAddTrusted.setBounds(285, 20, 80, 25);
		pane2_2.add(btnAddTrusted);

		btnRemoveTrusted = new JButton("Remove");
		btnRemoveTrusted.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeTrustedClick();
			}
		});
		btnRemoveTrusted.setBounds(285, 55, 80, 25);
		pane2_2.add(btnRemoveTrusted);

		JButton btnSign = new JButton("Sign");
		btnSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sign();
			}
		});
		btnSign.setBounds(697, 600, 80, 25);
		contentPanel.add(btnSign);
	}

	private String requestPassword() {
		JPasswordField jpf = new JPasswordField();
		int result = JOptionPane.showConfirmDialog(null, jpf, "Please enter password", JOptionPane.OK_CANCEL_OPTION);
		if (result == 0)
			return new String(jpf.getPassword());
		return "";
	}

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	protected void addSignClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			signer.getSigningChain().add(cert);
			updateSignCertificates();
		}
	}

	protected void removeSignClick()
	{
		signer.getSigningChain().remove(tableSign.getSelectedRow());
		((DefaultTableModel)tableSign.getModel()).removeRow(tableSign.getSelectedRow());
	}

	private void addSignCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)tableSign.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearSignCertificatesTable() {
		((DefaultTableModel)tableSign.getModel()).setNumRows(0);
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
			signer.setInputFile(edInputFile.getText());
			signer.setOutputFile(edOutputFile.getText());

			signer.setSigningCertificate(LoadCertificate(edSignCertFile.getText(), edSignCertPassword.getText()));

			if (signer.getSigningCertificate().getKeyAlgorithm().contains("id-dsa"))
			{
				showMessage("Info", "The certificate was found to contain a DSA key. The hash algorithm has been switched to SHA1.");

				signer.getNewSignature().setHashAlgorithm("SHA1");
			}

			switch (cmbLevel.getSelectedIndex())
			{
				case 0: signer.getNewSignature().setLevel(PDFSignature.pslLegacy); break;
				case 1: signer.getNewSignature().setLevel(PDFSignature.pslBES); break;
				case 2: signer.getNewSignature().setLevel(PDFSignature.pslEPES); break;
				case 3: signer.getNewSignature().setLevel(PDFSignature.pslLTV); break;
				case 4: signer.getNewSignature().setLevel(PDFSignature.pslDocumentTimestamp); break;
			}
			signer.getWidget().setInvisible(!cbVisible.isSelected());

			if (cbAutoCollectRevInfo.isSelected())
			{
				signer.config("AutoCollectRevocationInfo=true");
			}
			else
			{
				signer.config("AutoCollectRevocationInfo=false");
			}

			signer.setIgnoreChainValidationErrors(cbIgnoreChainValidationErrors.isSelected());

			if (cbForceCompleteChainValidation.isSelected())
			{
				signer.config("ForceCompleteChainValidation=true");
			}
			else
			{
				signer.config("ForceCompleteChainValidation=false");
			}

			if (cbDeepValidation.isSelected())
			{
				signer.config("DeepValidation=true");
			}
			else
			{
				signer.config("DeepValidation=false");
			}

			if (cbIncludeLocalRevInfo.isSelected())
			{
				signer.config("IncludeKnownRevocationInfoToSignature=true");
			}
			else
			{
				signer.config("IncludeKnownRevocationInfoToSignature=false");
			}

			if (cbIncludeRevocationInfoToAdbeAttribute.isSelected())
			{
				signer.config("IncludeRevocationInfoToAdbeAttribute=true");
			}
			else
			{
				signer.config("IncludeRevocationInfoToAdbeAttribute=false");
			}

			signer.getNewSignature().setPolicyID(edIdentifier.getText());
			signer.getNewSignature().setPolicyHashAlgorithm(cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));
			signer.getNewSignature().setPolicyHash(edHashValue.getText());

			signer.getNewSignature().setAuthorName(edAuthor.getText());
			signer.getNewSignature().setReason(edReason.getText());

			if (cbTimestamp.isSelected())
			{
				signer.setTimestampServer(edTimestampServer.getText());
			}
			else
			{
				signer.setTimestampServer("");
			}

			signer.sign();

			showMessage("Info", "PDF file successfully signed");
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



