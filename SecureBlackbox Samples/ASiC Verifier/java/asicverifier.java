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


class asicverifier extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputPath;

	private JComboBox cmbExtractionMode;
	private JCheckBox cbPerformRevocationCheck;
	private JCheckBox cbIgnoreChainValidationErrors;
	private JCheckBox cbForceCompleteChainValidation;

	private JTable tableKnown;
	private JButton btnAddKnown;
	private JButton btnRemoveKnown;
	private JTable tableTrusted;
	private JButton btnAddTrusted;
	private JButton btnRemoveTrusted;

	Asicverifier verifier;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			asicverifier dialog = new asicverifier();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public asicverifier()
	{
		verifier = new Asicverifier();
		try
		{
			verifier.addAsicverifierEventListener(new AsicverifierEventListener() {
				@Override
				public void chainElementDownload(AsicverifierChainElementDownloadEvent asicverifierChainElementDownloadEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainElementNeeded(AsicverifierChainElementNeededEvent asicverifierChainElementNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainElementStore(AsicverifierChainElementStoreEvent asicverifierChainElementStoreEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidated(AsicverifierChainValidatedEvent asicverifierChainValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidationProgress(AsicverifierChainValidationProgressEvent asicverifierChainValidationProgressEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void containerLoaded(AsicverifierContainerLoadedEvent asicverifierContainerLoadedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void error(AsicverifierErrorEvent asicverifierErrorEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void fileExtractionStart(AsicverifierFileExtractionStartEvent asicverifierFileExtractionStartEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(AsicverifierNotificationEvent asicverifierNotificationEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void signatureFound(AsicverifierSignatureFoundEvent e) {
					if (e.certFound) {
						e.validateSignature = true;
						e.validateChain = true;
					} else {
						signdialog frmSign = new signdialog(verifier, e.issuerRDN, e.serialNumber, e.subjectKeyID);
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
				public void signatureValidated(AsicverifierSignatureValidatedEvent asicverifierSignatureValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampFound(AsicverifierTimestampFoundEvent asicverifierTimestampFoundEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampValidated(AsicverifierTimestampValidatedEvent asicverifierTimestampValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSCertNeeded(AsicverifierTLSCertNeededEvent asicverifierTLSCertNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSCertValidate(AsicverifierTLSCertValidateEvent asicverifierTLSCertValidateEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSEstablished(AsicverifierTLSEstablishedEvent asicverifierTLSEstablishedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSHandshake(AsicverifierTLSHandshakeEvent asicverifierTLSHandshakeEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSShutdown(AsicverifierTLSShutdownEvent asicverifierTLSShutdownEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void supercoreIntercept(AsicverifierSupercoreInterceptEvent asicverifierSupercoreInterceptEvent) {
					// TODO Auto-generated method stub
				}
			});

		}
		catch (Exception e)
		{}

		setTitle("ASiC Verifier demo");

		setBounds(100, 100, 880, 385);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample shows how to process signed ASiC containers. Please select an ASiC file, tune up validation settings, and click 'Verify' when ready. ");
		lblCaption.setBounds(10, 5, 870, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);
		
		JLabel lblInputFile = new JLabel("Input File");
		lblInputFile.setBounds(10, 33, 70, 14);
		contentPanel.add(lblInputFile);

		edInputFile = new JTextField();
		edInputFile.setBounds(80, 30, 440, 20);
		contentPanel.add(edInputFile);
		edInputFile.setColumns(10);

		JButton sbBrowseInputFile = new JButton("Browse");
		sbBrowseInputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edInputFile.setText(getFileName());
			}
		});
		sbBrowseInputFile.setBounds(525, 28, 80, 25);
		contentPanel.add(sbBrowseInputFile);
		contentPanel.setLayout(null);

		JLabel lblOutputPath = new JLabel("Output Path");
		lblOutputPath.setBounds(10, 63, 70, 14);
		contentPanel.add(lblOutputPath);

		edOutputPath = new JTextField();
		edOutputPath.setBounds(80, 60, 440, 20);
		contentPanel.add(edOutputPath);
		edOutputPath.setColumns(10);

		JButton sbBrowseOutputPath = new JButton("Browse");
		sbBrowseOutputPath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edOutputPath.setText(selectDirectory());
			}
		});
		sbBrowseOutputPath.setBounds(525, 58, 80, 25);
		contentPanel.add(sbBrowseOutputPath);
		contentPanel.setLayout(null);


		JPanel Settingspanel = new JPanel();
		Settingspanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Validation settings  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Settingspanel.setBounds(10, 100, 845, 200);
		contentPanel.add(Settingspanel);
		Settingspanel.setLayout(null);

		JLabel lblExtractionMode = new JLabel("Extraction Mode");
		lblExtractionMode.setBounds(10, 28, 100, 14);
		Settingspanel.add(lblExtractionMode);

		cmbExtractionMode = new JComboBox<String>();
		cmbExtractionMode.setModel(new DefaultComboBoxModel<String>(new String[] {"None", "All", "Signed", "SignedAndValid"}));
		cmbExtractionMode.setBounds(110, 25, 210, 22);
		Settingspanel.add(cmbExtractionMode);

		cbPerformRevocationCheck = new JCheckBox("Perform revocation check");
		cbPerformRevocationCheck.setBounds(10, 55, 180, 23);
		Settingspanel.add(cbPerformRevocationCheck);

		cbIgnoreChainValidationErrors = new JCheckBox("Ignore chain validation errors");
		cbIgnoreChainValidationErrors.setBounds(210, 55, 200, 23);
		Settingspanel.add(cbIgnoreChainValidationErrors);

		cbForceCompleteChainValidation = new JCheckBox("Force complete chain validation");
		cbForceCompleteChainValidation.setBounds(430, 55, 240, 23);
		Settingspanel.add(cbForceCompleteChainValidation);

		JPanel Knownpanel = new JPanel();
		Knownpanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Knownpanel.setBounds(5, 85, 410, 110);
		Settingspanel.add(Knownpanel);
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
		Trustpanel.setBounds(430, 85, 410, 110);
		Settingspanel.add(Trustpanel);
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


		JButton btnSign = new JButton("Verify");
		btnSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				verify();
			}
		});
		btnSign.setBounds(770, 310, 80, 25);
		contentPanel.add(btnSign);
	}

	protected void verify() {
		try
		{
			verifier.setInputFile(edInputFile.getText());
			verifier.setOutputPath(edOutputPath.getText());

			switch (cmbExtractionMode.getSelectedIndex())
			{
				case 1:
					verifier.setExtractionMode(Asicverifier.aemAll);
					break;
				case 2:
					verifier.setExtractionMode(Asicverifier.aemSigned);
					break;
				case 3:
					verifier.setExtractionMode(Asicverifier.aemSignedAndValid);
					break;
				default:
					verifier.setExtractionMode(Asicverifier.aemNone);
					break;
			}

                        if (cbPerformRevocationCheck.isSelected()) 
				verifier.setRevocationCheck(1);
                        else
				verifier.setRevocationCheck(0);
                        
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

	private String selectDirectory() {
		JFileChooser fc = new JFileChooser();
		fc.setCurrentDirectory(new java.io.File("."));
		fc.setDialogTitle("Select directory to extract files to");
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fc.setAcceptAllFileFilterUsed(false);

		if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
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

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
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



