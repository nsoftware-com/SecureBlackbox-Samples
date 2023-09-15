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

class jadessigner extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edDataFile;
	private JTextField edOutputFile;
	private JCheckBox cbDetached;
	private JCheckBox cbCompactForm;
	private JCheckBox cbFlattenedSignature;

	private JComboBox<String> cmbLevel;
	private JTextField edSignCertFile;
	private JTextField edSignCertPassword;
	private JTable tableSign;
	private JButton btnAddSign;
	private JButton btnRemoveSign;

	private JCheckBox cbIgnoreChainValidationErrors;
	private JCheckBox cbForceCompleteChainValidation;
	private JTextField edIdentifier;
	private JComboBox<String> cmbHashAlgorithm;
	private JTextField edHashValue;
	private JCheckBox cbTimestamp;
	private JTextField edTimestampServer;

	private JTable tableKnown;
	private JButton btnAddKnown;
	private JButton btnRemoveKnown;
	private JTable tableTrusted;
	private JButton btnAddTrusted;
	private JButton btnRemoveTrusted;
	private JCheckBox cbIncludeLocalRevInfo;

	Jadessigner signer;
	Certificatemanager certmanager;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			jadessigner dialog = new jadessigner();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public jadessigner()
	{
		signer = new Jadessigner();
		certmanager = new Certificatemanager();

		setTitle("JAdES Signer");
		
		setBounds(100, 100, 815, 675);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates the use of JAdESSigner component for creating JWS/JAdES signature. Pick the signing certificate and click 'Sign'.");
		lblCaption.setBounds(10, 5, 790, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblDataFile = new JLabel("Data File");
		lblDataFile.setBounds(10, 33, 70, 14);
		contentPanel.add(lblDataFile);

		edDataFile = new JTextField();
		edDataFile.setBounds(80, 30, 310, 20);
		contentPanel.add(edDataFile);
		edDataFile.setColumns(10);

		JButton sbBrowseDataFile = new JButton("Browse");
		sbBrowseDataFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edDataFile.setText(getFileName());
			}
		});
		sbBrowseDataFile.setBounds(397, 28, 80, 25);
		contentPanel.add(sbBrowseDataFile);
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

		cbDetached = new JCheckBox("Detached");
		cbDetached.setBounds(500, 25, 150, 23);
		contentPanel.add(cbDetached);

		cbCompactForm = new JCheckBox("Compact Form");
		cbCompactForm.setBounds(500, 45, 150, 23);
		contentPanel.add(cbCompactForm);

		cbFlattenedSignature = new JCheckBox("Flattened Signature");
		cbFlattenedSignature.setBounds(500, 65, 150, 23);
		cbFlattenedSignature.setSelected(true);
		contentPanel.add(cbFlattenedSignature);
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
		cmbLevel.setModel(new DefaultComboBoxModel<String>(new String[] {"JWS", "Baseline-B", "Baseline-T", "Baseline-LT", "Baseline-LTA"}));
		cmbLevel.setSelectedIndex(1);
		cmbLevel.setBounds(50, 25, 100, 22);
		panelOptions.add(cmbLevel);

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
		panelAddOptions.setBounds(5, 265, 775, 130);
		contentPanel.add(panelAddOptions);
		panelAddOptions.setLayout(null);

		cbIgnoreChainValidationErrors = new JCheckBox("Ignore chain validation errors");
		cbIgnoreChainValidationErrors.setBounds(395, 80, 350, 23);
		panelAddOptions.add(cbIgnoreChainValidationErrors);

		cbForceCompleteChainValidation = new JCheckBox("Force complete chain validation");
		cbForceCompleteChainValidation.setBounds(395, 100, 210, 23);
		panelAddOptions.add(cbForceCompleteChainValidation);

		JPanel pane1 = new JPanel();
		pane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Policy", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane1.setBounds(5, 15, 375, 110);
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
		cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"", "SHA256", "SHA384", "SHA512"}));
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
		cbTimestamp.setBounds(395, 25, 250, 23);
		panelAddOptions.add(cbTimestamp);

		edTimestampServer = new JTextField();
		edTimestampServer.setText("http://");
		edTimestampServer.setBounds(405, 50, 270, 20);
		panelAddOptions.add(edTimestampServer);
		edTimestampServer.setColumns(10);

		JPanel pane2 = new JPanel();
		pane2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Revocation information", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane2.setBounds(5, 405, 775, 160);
		contentPanel.add(pane2);
		pane2.setLayout(null);

		cbIncludeLocalRevInfo = new JCheckBox("Include local revocation information to the signature");
		cbIncludeLocalRevInfo.setBounds(10, 25, 350, 23);
		pane2.add(cbIncludeLocalRevInfo);

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
		btnSign.setBounds(697, 570, 80, 25);
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
			signer.setDataFile(edDataFile.getText());
			signer.setOutputFile(edOutputFile.getText());

			signer.setDetached(cbDetached.isSelected());
			signer.getNewSignature().setSignedDataType(JAdESSignature.jasdtPayload);

			signer.setCompactForm(cbCompactForm.isSelected());
			signer.setFlattenedSignature(cbFlattenedSignature.isSelected());

			signer.setSigningCertificate(LoadCertificate(edSignCertFile.getText(), edSignCertPassword.getText()));

			switch (cmbLevel.getSelectedIndex())
			{
				case 0: signer.getNewSignature().setLevel(JAdESSignature.jaslJWS); break;
				case 1: signer.getNewSignature().setLevel(JAdESSignature.jaslBaselineB); break;
				case 2: signer.getNewSignature().setLevel(JAdESSignature.jaslBaselineT); break;
				case 3: signer.getNewSignature().setLevel(JAdESSignature.jaslBaselineLT); break;
				case 4: signer.getNewSignature().setLevel(JAdESSignature.jaslBaselineLTA); break;
			}

			signer.getNewSignature().setHashAlgorithm("SHA256");

			signer.setIgnoreChainValidationErrors(cbIgnoreChainValidationErrors.isSelected());

			if (cbForceCompleteChainValidation.isSelected())
			{
				signer.config("ForceCompleteChainValidation=true");
			}
			else
			{
				signer.config("ForceCompleteChainValidation=false");
			}

			if (cbIncludeLocalRevInfo.isSelected())
			{
				signer.config("IncludeKnownRevocationInfoToSignature=true");
			}
			else
			{
				signer.config("IncludeKnownRevocationInfoToSignature=false");
			}

			signer.getNewSignature().setPolicyID(edIdentifier.getText());
			signer.getNewSignature().setPolicyHashAlgorithm(cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));
			signer.getNewSignature().setPolicyHash(edHashValue.getText());

			if (cbTimestamp.isSelected()) {
				if ((signer.getNewSignature().getLevel() == JAdESSignature.jaslBaselineB) ||
						(signer.getNewSignature().getLevel() == JAdESSignature.jaslJWS)) {
					signer.getNewSignature().setLevel(JAdESSignature.jaslBaselineT);
				}

				signer.setTimestampServer(edTimestampServer.getText());
			}
			else
			{
				signer.setTimestampServer("");
			}

			signer.sign();

			showMessage("Info", "JWS/JAdES signature successfully created");
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
				certmanager.importFromFile(file, password);
				cert = certmanager.getCertificate();
			}
			catch (Exception e)
			{
				showErrorMessage("Failed to load certificate: " + e.getMessage());
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



