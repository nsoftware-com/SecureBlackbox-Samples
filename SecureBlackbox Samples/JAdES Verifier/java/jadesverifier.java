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

class jadesverifier extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edDataFile;

	private JCheckBox cbPerformRevocationCheck;
	private JCheckBox cbIgnoreChainValidationErrors;
	private JCheckBox cbForceCompleteChainValidation;

	private JTable tableKnown;
	private JButton btnAddKnown;
	private JButton btnRemoveKnown;
	private JTable tableTrusted;
	private JButton btnAddTrusted;
	private JButton btnRemoveTrusted;

	Jadesverifier verifier;
	Certificatemanager certmanager;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			jadesverifier dialog = new jadesverifier();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public jadesverifier()
	{
		verifier = new Jadesverifier();
		try {
			verifier.addJadesverifierEventListener(new JadesverifierEventListener() {
				@Override
				public void chainElementDownload(JadesverifierChainElementDownloadEvent jadesverifierChainElementDownloadEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainElementNeeded(JadesverifierChainElementNeededEvent jadesverifierChainElementNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainElementStore(JadesverifierChainElementStoreEvent jadesverifierChainElementStoreEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidated(JadesverifierChainValidatedEvent jadesverifierChainValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidationProgress(JadesverifierChainValidationProgressEvent jadesverifierChainValidationProgressEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void error(JadesverifierErrorEvent jadesverifierErrorEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void HTTPHeaderFieldNeeded(JadesverifierHTTPHeaderFieldNeededEvent jadesverifierHTTPHeaderFieldNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void loaded(JadesverifierLoadedEvent jadesverifierLoadedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(JadesverifierNotificationEvent jadesverifierNotificationEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void objectNeeded(JadesverifierObjectNeededEvent jadesverifierObjectNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void objectValidate(JadesverifierObjectValidateEvent jadesverifierObjectValidateEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void signatureFound(JadesverifierSignatureFoundEvent e) {
					if (e.certFound) {
						e.validateSignature = true;
						e.validateChain = true;
					} else {
						signdialog frmSign = new signdialog(verifier, e.index, e.issuerRDN, e.serialNumber, e.subjectKeyID);
						try {
							frmSign.setModal(true);
							frmSign.setVisible(true);

							if (frmSign.isOK()) {
								e.validateSignature = true;
								e.validateChain = true;
							} else {
								e.validateSignature = false;
								e.validateChain = false;
							}
						} finally {
							updateKnownCertificates();
							frmSign.dispose();
						}
					}
				}

				@Override
				public void signatureValidated(JadesverifierSignatureValidatedEvent jadesverifierSignatureValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampFound(JadesverifierTimestampFoundEvent jadesverifierTimestampFoundEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampValidated(JadesverifierTimestampValidatedEvent jadesverifierTimestampValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSCertNeeded(JadesverifierTLSCertNeededEvent jadesverifierTLSCertNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSCertValidate(JadesverifierTLSCertValidateEvent jadesverifierTLSCertValidateEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSEstablished(JadesverifierTLSEstablishedEvent jadesverifierTLSEstablishedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSHandshake(JadesverifierTLSHandshakeEvent jadesverifierTLSHandshakeEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSShutdown(JadesverifierTLSShutdownEvent jadesverifierTLSShutdownEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void supercoreIntercept(JadesverifierSupercoreInterceptEvent jadesverifierSupercoreInterceptEvent) {
					// TODO Auto-generated method stub
				}
			});
		}
		catch (Exception e)
		{}

		certmanager = new Certificatemanager();

		setTitle("JAdES Verifier");

		setBounds(100, 100, 880, 360);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates the use of JAdESVerifier component for validating JAdES signatures.");
		lblCaption.setBounds(10, 5, 870, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblInputFile = new JLabel("Input File");
		lblInputFile.setBounds(10, 38, 70, 14);
		contentPanel.add(lblInputFile);

		edInputFile = new JTextField();
		edInputFile.setBounds(80, 35, 440, 20);
		contentPanel.add(edInputFile);
		edInputFile.setColumns(10);

		JButton sbBrowseInputFile = new JButton("Browse");
		sbBrowseInputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edInputFile.setText(getFileName());
			}
		});
		sbBrowseInputFile.setBounds(525, 33, 80, 25);
		contentPanel.add(sbBrowseInputFile);

		JLabel lblDataFile = new JLabel("Data File");
		lblDataFile.setBounds(10, 68, 70, 14);
		contentPanel.add(lblDataFile);

		edDataFile = new JTextField();
		edDataFile.setBounds(80, 65, 440, 20);
		contentPanel.add(edDataFile);

		JButton sbBrowseDataFile = new JButton("Browse");
		sbBrowseDataFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edDataFile.setText(getFileName());
			}
		});
		sbBrowseDataFile.setBounds(525, 63, 80, 25);
		contentPanel.add(sbBrowseDataFile);
		contentPanel.setLayout(null);

		JPanel Settingspanel = new JPanel();
		Settingspanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Validation settings  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Settingspanel.setBounds(10, 100, 845, 175);
		contentPanel.add(Settingspanel);
		Settingspanel.setLayout(null);

		cbPerformRevocationCheck = new JCheckBox("Perform revocation check");
		cbPerformRevocationCheck.setBounds(10, 25, 180, 23);
		Settingspanel.add(cbPerformRevocationCheck);

		cbIgnoreChainValidationErrors = new JCheckBox("Ignore chain validation errors");
		cbIgnoreChainValidationErrors.setBounds(210, 25, 200, 23);
		Settingspanel.add(cbIgnoreChainValidationErrors);

		cbForceCompleteChainValidation = new JCheckBox("Force complete chain validation");
		cbForceCompleteChainValidation.setBounds(430, 25, 240, 23);
		Settingspanel.add(cbForceCompleteChainValidation);

		JPanel Knownpanel = new JPanel();
		Knownpanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Knownpanel.setBounds(5, 60, 410, 110);
		Settingspanel.add(Knownpanel);
		Knownpanel.setLayout(null);

		JScrollPane scrollPane2 = new JScrollPane();
		scrollPane2.setBounds(5, 20, 310, 85);
		Knownpanel.add(scrollPane2);

		tableKnown = new JTable();
		tableKnown.setModel(new DefaultTableModel(
				new Object[][]{
				},
				new String[]{
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
		Trustpanel.setBounds(430, 60, 410, 110);
		Settingspanel.add(Trustpanel);
		Trustpanel.setLayout(null);

		JScrollPane scrollPane3 = new JScrollPane();
		scrollPane3.setBounds(5, 20, 310, 85);
		Trustpanel.add(scrollPane3);

		tableTrusted = new JTable();
		tableTrusted.setModel(new DefaultTableModel(
				new Object[][]{
				},
				new String[]{
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


		JButton btnVerify = new JButton("Verify");
		btnVerify.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				verify();
			}
		});
		btnVerify.setBounds(773, 285, 80, 25);
		contentPanel.add(btnVerify);
	}

	protected void verify() {
		try
		{
			verifier.setInputFile(edInputFile.getText());
			verifier.setDataFile(edDataFile.getText());

			if (cbPerformRevocationCheck.isSelected())
				verifier.setRevocationCheck(Jadesverifier.crcAuto);
			else
				verifier.setRevocationCheck(Jadesverifier.crcNone);

			verifier.setIgnoreChainValidationErrors(cbIgnoreChainValidationErrors.isSelected());

			if (cbForceCompleteChainValidation.isSelected())
				verifier.config("ForceCompleteChainValidation=true");
			else
				verifier.config("ForceCompleteChainValidation=False");

			verifier.verify();

			DisplaySignaturesInfo();
		}
		catch (Exception ex) {
			showErrorMessage(ex.getMessage());
		}
	}

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	public void DisplaySignaturesInfo()
	{
		validationresultdialog frmRes = new validationresultdialog();
		frmRes.Init(verifier);
		frmRes.setLocationRelativeTo(this);
		frmRes.setModal(true);
		frmRes.setVisible(true);

		frmRes.dispose();
	}

	String getFileName(){
		JFileChooser fc = new JFileChooser();

		int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	private String requestPassword() {
		JPasswordField jpf = new JPasswordField();
		int result = JOptionPane.showConfirmDialog(null, jpf, "Please enter password", JOptionPane.OK_CANCEL_OPTION);
		if (result == 0)
			return new String(jpf.getPassword());
		return "";
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
				showErrorMessage("Cannot load certificate!");
			}
		}

		return cert;
	}

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	protected void addKnownClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			verifier.getKnownCertificates().add(cert);
			updateKnownCertificates();
		}
	}

	protected void removeKnownClick()
	{
		verifier.getKnownCertificates().remove(tableKnown.getSelectedRow());
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

		for (int i = 0; i < verifier.getKnownCertificates().size(); i++)
		{
			String s = verifier.getKnownCertificates().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addKnownCertItem(bytesToHex(verifier.getKnownCertificates().item(i).getSerialNumber()), s);
		}
	}

	protected void addTrustedClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			verifier.getTrustedCertificates().add(cert);
			updateTrustedCertificates();
		}
	}

	protected void removeTrustedClick()
	{
		verifier.getTrustedCertificates().remove(tableTrusted.getSelectedRow());
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

		for (int i = 0; i < verifier.getTrustedCertificates().size(); i++)
		{
			String s = verifier.getTrustedCertificates().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addTrustedCertItem(bytesToHex(verifier.getTrustedCertificates().item(i).getSerialNumber()), s);
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



