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

import static secureblackbox.Messageverifier.*;

class messageverifier extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JCheckBox cbDetached;
	private JLabel lblOutputFile;
	private JTextField edOutputFile;
	private JButton sbBrowseOutputFile;
	private JTable table;

	Messageverifier verifier;
	ArrayList referenceResult = new ArrayList();
	String Timestamp;
	String TimestampSerial;


	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			messageverifier dialog = new messageverifier();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public messageverifier()
	{
		verifier = new Messageverifier();
		try {
			verifier.addMessageverifierEventListener(new MessageverifierEventListener() {
				
				@Override
				public void timestampValidated(MessageverifierTimestampValidatedEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void timestampFound(MessageverifierTimestampFoundEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void supercoreIntercept(MessageverifierSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void signatureValidated(MessageverifierSignatureValidatedEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void signatureFound(MessageverifierSignatureFoundEvent e) {
					e.validateChain = false; // not support certificate chain validation.

					if (e.certFound) {
						e.validateSignature = true;
					} else {
						signdialog frmSign = new signdialog(verifier, e.issuerRDN, e.serialNumber, e.subjectKeyID);
						try {
							frmSign.setModal(true);
							frmSign.setVisible(true);

							if (frmSign.isOK()) {
								e.validateSignature = true;
							} else {
								e.validateSignature = false;
							}
						} finally {
							frmSign.dispose();
						}
					}
				}
				
				@Override
				public void error(MessageverifierErrorEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void notification(MessageverifierNotificationEvent messageverifierNotificationEvent) {
					// TODO Auto-generated method stub										
				}
			});
		} catch (Exception e) {
		}

		setTitle("Message Verifier");

		setBounds(100, 100, 600, 325);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample showcases MessageVerifier's facilities in validating PKCS#7-compliant signed files.");
		lblCaption.setBounds(10, 5, 590, 14);
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
		sbBrowseInputFile.setBounds(495, 33, 80, 25);
		contentPanel.add(sbBrowseInputFile);
		contentPanel.setLayout(null);

		cbDetached = new JCheckBox("Detached");
		cbDetached.setSelected(false);
		cbDetached.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (cbDetached.isSelected())
					lblOutputFile.setText("Data File");
				else
					lblOutputFile.setText("Output File");
			}
		});
		cbDetached.setBounds(5, 60, 166, 23);
		contentPanel.add(cbDetached);

		lblOutputFile = new JLabel("Output File");
		lblOutputFile.setBounds(10, 93, 61, 14);
		contentPanel.add(lblOutputFile);

		edOutputFile = new JTextField();
		edOutputFile.setBounds(74, 90, 415, 20);
		contentPanel.add(edOutputFile);
		edOutputFile.setColumns(10);

		sbBrowseOutputFile = new JButton("Browse");
		sbBrowseOutputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (cbDetached.isSelected())
					edOutputFile.setText(getFileName());
				else
					edOutputFile.setText(getSaveFileName());
			}
		});
		sbBrowseOutputFile.setBounds(495, 88, 80, 25);
		contentPanel.add(sbBrowseOutputFile);
		contentPanel.setLayout(null);

		JPanel panel_2 = new JPanel();
		panel_2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_2.setBounds(5, 120, 575, 120);
		contentPanel.add(panel_2);
		panel_2.setLayout(null);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 20, 470, 95);
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
		btnAdd.setBounds(485, 20, 80, 25);
		panel_2.add(btnAdd);

		JButton btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeClick();
			}
		});
		btnRemove.setBounds(485, 55, 80, 25);
		panel_2.add(btnRemove);


		JButton btnVerify = new JButton("Verify");
		btnVerify.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				verify();
			}
		});
		btnVerify.setBounds(497, 250, 80, 25);
		contentPanel.add(btnVerify);
	}

	protected void verify() {
		try
		{
			verifier.setInputFile(edInputFile.getText());

			if (cbDetached.isSelected())
			{
				verifier.setDataFile(edOutputFile.getText());
				verifier.verifyDetached();
			}
			else
			{
				verifier.setOutputFile(edOutputFile.getText());
				verifier.verify();
			}

			ShowResults();
		}
		catch (Exception ex) {
			showErrorMessage(ex.getMessage());
		}
	}

	private void ShowResults()
	{
		verifierresults VerifierResults = new verifierresults();
		try
		{
			StringBuilder sb = new StringBuilder();
			switch (verifier.getSignatureValidationResult())
			{
				case svtCorrupted:
					sb.append("Verification error: Corrupted\r\n");
					break;
				case svtFailure:
					sb.append("Verification error: Failure\r\n");
					break;
				case svtSignerNotFound:
					sb.append("Verification error: SignerNotFound\r\n");
					break;
				case svtUnknown:
					sb.append("Verification error: Unknown\r\n");
					break;
				default:
					sb.append("Successfully verified!\r\n");
					sb.append("Hash Algorithm: ");
					sb.append(verifier.getHashAlgorithm());
					sb.append("\r\n");
					sb.append("Certificates contained in message:\r\n");
					sb.append(GetCertificatesInfo(verifier.getCertificates()));
					break;
			}

			VerifierResults.setResult(sb.toString());

			VerifierResults.setLocationRelativeTo(this);

			VerifierResults.setModal(true);
			VerifierResults.setVisible(true);
		}
		finally
		{
			VerifierResults.dispose();
		}
	}

	private String GetCertificatesInfo(CertificateList storage)
	{
		StringBuilder sb = new StringBuilder();
		int iCount = storage.size();
		for (int i = 0; i < iCount; i++)
		{
			Certificate cert = storage.item(i);
			sb.append("Certificate #");
			sb.append(i + 1);
			sb.append("\r\n");
			sb.append("Issuer: ");
			sb.append(cert.getIssuer());
			sb.append("\r\n");
			sb.append("Subject: ");
			sb.append(cert.getSubject());
			sb.append("\r\n");
			if (cert.getPrivateKeyExists())
				sb.append("Private key available\r\n");
			else
				sb.append("Private key is not available\r\n");
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

	private String requestPassword() {
		JPasswordField jpf = new JPasswordField();
		int result = JOptionPane.showConfirmDialog(null, jpf, "Please enter password", JOptionPane.OK_CANCEL_OPTION);
		if (result == 0)
			return new String(jpf.getPassword());
		return "";
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

	private void addSignCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)table.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearSigningCertificatesTable() {
		((DefaultTableModel)table.getModel()).setNumRows(0);
	}

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	private void updateCertificates() {
		clearSigningCertificatesTable();

		for (int i = 0; i < verifier.getKnownCertificates().size(); i++)
		{
			String s = verifier.getKnownCertificates().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addSignCertItem(bytesToHex(verifier.getKnownCertificates().item(i).getSerialNumber()), s);
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



