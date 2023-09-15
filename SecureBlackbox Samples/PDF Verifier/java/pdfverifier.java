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


class pdfverifier extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;

	private JCheckBox cbPerformRevocationCheck;
	private JCheckBox cbIgnoreChainValidationErrors;
	private JCheckBox cbForceCompleteChainValidation;
	private JCheckBox cbDeepValidation;

	private JTable tableKnown;
	private JButton btnAddKnown;
	private JButton btnRemoveKnown;
	private JTable tableTrusted;
	private JButton btnAddTrusted;
	private JButton btnRemoveTrusted;

	Pdfverifier verifier;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			pdfverifier dialog = new pdfverifier();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public pdfverifier()
	{
		verifier = new Pdfverifier();
		try {
			verifier.addPdfverifierEventListener(new PdfverifierEventListener() {
				@Override
				public void chainElementDownload(PdfverifierChainElementDownloadEvent pdfverifierChainElementDownloadEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainElementNeeded(PdfverifierChainElementNeededEvent pdfverifierChainElementNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidated(PdfverifierChainValidatedEvent pdfverifierChainValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidationProgress(PdfverifierChainValidationProgressEvent pdfverifierChainValidationProgressEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void documentLoaded(PdfverifierDocumentLoadedEvent pdfverifierDocumentLoadedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void encrypted(PdfverifierEncryptedEvent pdfverifierEncryptedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void error(PdfverifierErrorEvent pdfverifierErrorEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(PdfverifierNotificationEvent pdfverifierNotificationEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void signatureFound(PdfverifierSignatureFoundEvent e) {
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
				public void signatureValidated(PdfverifierSignatureValidatedEvent pdfverifierSignatureValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampFound(PdfverifierTimestampFoundEvent pdfverifierTimestampFoundEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampValidated(PdfverifierTimestampValidatedEvent pdfverifierTimestampValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSCertNeeded(PdfverifierTLSCertNeededEvent pdfverifierTLSCertNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSCertValidate(PdfverifierTLSCertValidateEvent pdfverifierTLSCertValidateEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSEstablished(PdfverifierTLSEstablishedEvent pdfverifierTLSEstablishedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSHandshake(PdfverifierTLSHandshakeEvent pdfverifierTLSHandshakeEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSShutdown(PdfverifierTLSShutdownEvent pdfverifierTLSShutdownEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void supercoreIntercept(PdfverifierSupercoreInterceptEvent pdfverifierSupercoreInterceptEvent) {
					// TODO Auto-generated method stub
				}
			});
		}
		catch (Exception e)
		{}

		setTitle("PDF Verifier");

		setBounds(100, 100, 880, 330);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates the use of PDFVerifier component for validating PDF signatures. ");
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
		contentPanel.setLayout(null);

		JPanel Settingspanel = new JPanel();
		Settingspanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Validation settings  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Settingspanel.setBounds(10, 70, 845, 175);
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

		cbDeepValidation = new JCheckBox("Deep validation");
		cbDeepValidation.setBounds(670, 25, 120, 23);
		Settingspanel.add(cbDeepValidation);

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
		btnVerify.setBounds(773, 255, 80, 25);
		contentPanel.add(btnVerify);
	}

	protected void verify() {
		try
		{
			verifier.setInputFile(edInputFile.getText());

			if (cbPerformRevocationCheck.isSelected())
				verifier.setRevocationCheck(Pdfverifier.crcAuto);
			else
				verifier.setRevocationCheck(Pdfverifier.crcNone);

			verifier.setIgnoreChainValidationErrors(cbIgnoreChainValidationErrors.isSelected());

			if (cbForceCompleteChainValidation.isSelected())
				verifier.config("ForceCompleteChainValidation=true");
			else
				verifier.config("ForceCompleteChainValidation=False");

			if (cbDeepValidation.isSelected())
				verifier.config("DeepValidation=true");
			else
				verifier.config("DeepValidation=False");

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



