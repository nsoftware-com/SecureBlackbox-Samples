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
import java.util.ArrayList;

import secureblackbox.*;

class xadesverifier extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JCheckBox cbDetached;
	private JLabel lblOutputFile;
	private JTextField edDataFile;
	private JButton sbBrowseDataFile;
	private JTable table;

	Xadesverifier verifier;
	ArrayList referenceResult = new ArrayList();
	String Timestamp;
	String TimestampSerial;


	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			xadesverifier dialog = new xadesverifier();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	/**
	 * Create the dialog.
	 */
	public xadesverifier()
	{
		verifier = new Xadesverifier();
		try
		{
			verifier.addXadesverifierEventListener(new XadesverifierEventListener() {
				@Override
				public void chainElementDownload(XadesverifierChainElementDownloadEvent xadesverifierChainElementDownloadEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainElementNeeded(XadesverifierChainElementNeededEvent xadesverifierChainElementNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainElementStore(XadesverifierChainElementStoreEvent xadesverifierChainElementStoreEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidated(XadesverifierChainValidatedEvent xadesverifierChainValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void chainValidationProgress(XadesverifierChainValidationProgressEvent xadesverifierChainValidationProgressEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void documentLoaded(XadesverifierDocumentLoadedEvent xadesverifierDocumentLoadedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void error(XadesverifierErrorEvent xadesverifierErrorEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(XadesverifierNotificationEvent xadesverifierNotificationEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void referenceValidated(XadesverifierReferenceValidatedEvent e) {
					referenceres item = new referenceres(e.ID, e.URI, e.refType, e.digestValid);
					referenceResult.add(item);
				}

				@Override
				public void resolveReference(XadesverifierResolveReferenceEvent xadesverifierResolveReferenceEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void signatureFound(XadesverifierSignatureFoundEvent e) {
					if (e.certFound) {
						e.validateSignature = true;
						e.validateChain = true;
					} else {
						signdialog frmSign = new signdialog(verifier, e.index);
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
							frmSign.dispose();
						}
					}
				}

				@Override
				public void signatureValidated(XadesverifierSignatureValidatedEvent xadesverifierSignatureValidatedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampFound(XadesverifierTimestampFoundEvent xadesverifierTimestampFoundEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void timestampValidated(XadesverifierTimestampValidatedEvent e) {
					if (e.validationResult == TimestampInfo.cvtValid) {
						Timestamp = "Timestamp: " + e.time;
						TimestampSerial = "Timestamp Serial: " + bytesToHex(e.serialNumber);
					} else {
						Timestamp = "Timestamp signature is not valid";
						TimestampSerial = "";
					}
				}

				@Override
				public void TLSCertNeeded(XadesverifierTLSCertNeededEvent xadesverifierTLSCertNeededEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSCertValidate(XadesverifierTLSCertValidateEvent xadesverifierTLSCertValidateEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSEstablished(XadesverifierTLSEstablishedEvent xadesverifierTLSEstablishedEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSHandshake(XadesverifierTLSHandshakeEvent xadesverifierTLSHandshakeEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void TLSShutdown(XadesverifierTLSShutdownEvent xadesverifierTLSShutdownEvent) {
					// TODO Auto-generated method stub
				}

				@Override
				public void supercoreIntercept(XadesverifierSupercoreInterceptEvent xadesverifierSupercoreInterceptEvent) {
					// TODO Auto-generated method stub
				}
			});
		}
		catch (Exception e)
		{}

		setTitle("XAdES Verifier");

		setBounds(100, 100, 610, 330);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample shows processing of XAdES signatures. Please select a signed XML file and click 'Verify'.");
		lblCaption.setBounds(10, 5, 600, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		contentPanel.add(lblCaption);

		JLabel lblInputFile = new JLabel("Input File");
		lblInputFile.setBounds(10, 38, 61, 14);
		contentPanel.add(lblInputFile);

		edInputFile = new JTextField();
		edInputFile.setBounds(74, 35, 415, 20);
		contentPanel.add(edInputFile);
		edInputFile.setColumns(10);

		JButton sbBrowseInputFile = new JButton("Browse");
		sbBrowseInputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edInputFile.setText(getFileName());
			}
		});
		sbBrowseInputFile.setBounds(507, 33, 80, 25);
		contentPanel.add(sbBrowseInputFile);
		contentPanel.setLayout(null);

		cbDetached = new JCheckBox("Detached");
		cbDetached.setSelected(false);
		cbDetached.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				edDataFile.setEnabled(cbDetached.isSelected());
				sbBrowseDataFile.setEnabled(cbDetached.isSelected());
			}
		});
		cbDetached.setBounds(5, 60, 166, 23);
		contentPanel.add(cbDetached);

		lblOutputFile = new JLabel("Data File");
		lblOutputFile.setBounds(10, 93, 61, 14);
		contentPanel.add(lblOutputFile);

		edDataFile = new JTextField();
		edDataFile.setBounds(74, 90, 415, 20);
		edDataFile.setEnabled(false);
		contentPanel.add(edDataFile);
		edDataFile.setColumns(10);

		sbBrowseDataFile = new JButton("Browse");
		sbBrowseDataFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edDataFile.setText(getFileName());
			}
		});
		sbBrowseDataFile.setBounds(507, 88, 80, 25);
		sbBrowseDataFile.setEnabled(false);
		contentPanel.add(sbBrowseDataFile);
		contentPanel.setLayout(null);

		JPanel panel_2 = new JPanel();
		panel_2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_2.setBounds(5, 120, 585, 120);
		contentPanel.add(panel_2);
		panel_2.setLayout(null);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 20, 485, 95);
		panel_2.add(scrollPane);

		table = new JTable();
		table.setModel(new DefaultTableModel(
				new Object[][] {
				},
				new String[] {
						"Serial", "Issuer"
				}
		));
		table.setFillsViewportHeight(true);
		scrollPane.setViewportView(table);

		JButton btnAdd = new JButton("Add");
		btnAdd.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addClick();
			}
		});
		btnAdd.setBounds(495, 20, 80, 25);
		panel_2.add(btnAdd);

		JButton btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeClick();
			}
		});
		btnRemove.setBounds(495, 55, 80, 25);
		panel_2.add(btnRemove);


		JButton btnVerify = new JButton("Verify");
		btnVerify.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				verify();
			}
		});
		btnVerify.setBounds(507, 250, 80, 25);
		contentPanel.add(btnVerify);
	}

	protected void verify() {
		try {
			referenceResult.clear();
			Timestamp = "";
			TimestampSerial = "";

			verifier.getKnownCertificates().clear();
			verifier.setInputFile(edInputFile.getText());

			if (cbDetached.isSelected()) {
				verifier.setDataFile(edDataFile.getText());
				verifier.setDataType(Xmlverifier.cxdtBinary);
				File f = new File(edDataFile.getText());
				verifier.setDataURI(f.getName());

				verifier.verifyDetached();
			}
			else
				verifier.verify();

			if (verifier.getSignatures().size() == 0)
				showErrorMessage("No signatures found");

			for (Integer i = 0; i < verifier.getSignatures().size(); i++) {
				XAdESSignature sig = verifier.getSignatures().get(i);
				switch (sig.getSignatureValidationResult()) {
					case XAdESSignature.xsvSignerNotFound: {
						showErrorMessage("Signer not found");
						break;
					}
					case XAdESSignature.xsvFailure: {
						showErrorMessage("Signature verification failed");
						break;
					}
					case XAdESSignature.xsvCorrupted: {
						showErrorMessage("Signature is invalid");
						break;
					}
					case XAdESSignature.xsvReferenceCorrupted: {
						if (chooseMessage("Signature has invalid references.\r\nDo you want to view reference validation results?")) {
							showReferencesDialog();
						}
						break;
					}
					case XAdESSignature.xsvValid: {
						Boolean SigOK = true;

						if (sig.getChainValidationResult() == XAdESSignature.cvtValidButUntrusted)
							showMessage("The selected signature is signed by self-signed certificate which was not previously trusted");
						else if (sig.getChainValidationResult() != XAdESSignature.cvtValid)
						{
							//SigOK = false;
							if (sig.getChainValidationResult() == XAdESSignature.cvtCantBeEstablished)
								showErrorMessage("Signing certificate chain could not be validated completely.");
							else
								showErrorMessage("Signing certificate is not valid.");
						}

						if (SigOK) {
							if (chooseMessage("Signature validated successfully.\r\nnDo you want to view reference validation results?")) {
								showReferencesDialog();
							}

							if (sig.getXAdES()) {
								if (chooseMessage("Do you want to view XAdES information?")) {
									xades frmXAdES = new xades(verifier, i, Timestamp, TimestampSerial);
									try {
										frmXAdES.setLocationRelativeTo(this);

										frmXAdES.setModal(true);
										frmXAdES.setVisible(true);
									} finally {
										frmXAdES.dispose();
									}
								}
							}
						}
						break;
					}
					default:
						showErrorMessage("Unknown validation result");
						break;
				}
			}
		}
		catch (Exception ex) {
			showErrorMessage(ex.getMessage());
		}
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

	private String requestPassword() {
		JPasswordField jpf = new JPasswordField();
		int result = JOptionPane.showConfirmDialog(null, jpf, "Please enter password", JOptionPane.OK_CANCEL_OPTION);
		if (result == 0)
			return new String(jpf.getPassword());
		return "";
	}

	private void addKnownCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)table.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearKnownCertificatesTable() {
		((DefaultTableModel)table.getModel()).setNumRows(0);
	}

	private void updateCertificates() {
		clearKnownCertificatesTable();

		for (int i = 0; i < verifier.getKnownCertificates().size(); i++)
		{
			String s = verifier.getKnownCertificates().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addKnownCertItem(bytesToHex(verifier.getKnownCertificates().item(i).getSerialNumber()), s);
		}
	}

	protected void addClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			verifier.getKnownCertificates().add(cert);
			updateCertificates();
		}
	}

	protected void removeClick() {
		verifier.getKnownCertificates().remove(table.getSelectedRow());
		((DefaultTableModel)table.getModel()).removeRow(table.getSelectedRow());
	}

	private boolean chooseMessage(String msg) {
		return (JOptionPane.showConfirmDialog(this, msg) == JOptionPane.YES_OPTION);
	}

	static void showMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	public void showReferencesDialog()
	{
		refsdialog frmReferences = new refsdialog(referenceResult);
		frmReferences.setLocationRelativeTo(this);
		frmReferences.setModal(true);
		frmReferences.setVisible(true);

		frmReferences.dispose();
	}

	String getFileName(){
		JFileChooser fc = new JFileChooser();

		int returnVal = fc.showOpenDialog(this);
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



