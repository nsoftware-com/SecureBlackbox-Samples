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

import static secureblackbox.Officesigner.*;

class officesigner extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;

	private JTextField edSignCertFile;
	private JTextField edSignCertPassword;
	private JComboBox<String> cmbSignatureType;
	private JComboBox<String> cmbHashAlgorithm;
	private JCheckBox cbSignDocument;
	private JCheckBox cbSignSignatureOrigin;
	private JCheckBox cbSignCoreProperties;
	private JCheckBox cbIncludeSigInfo;
	private JTextField edSigText;
	private JTextField edSigComments;

	private JTable tableSigningChain;
	private JButton btnAdd;
	private JButton btnRemove;

	Officesigner signer;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			officesigner dialog = new officesigner();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public officesigner()
	{
		signer = new Officesigner();

		setTitle("Office Signer");
		
		setBounds(100, 100, 700, 485);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates the use of OfficeSigner component to sign office documents. ");
		lblCaption.setBounds(10, 5, 690, 14);
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
		panelOptions.setBounds(5, 90, 675, 165);
		contentPanel.add(panelOptions);
		panelOptions.setLayout(null);

		JLabel lblSignatureType = new JLabel("Signature Type");
		lblSignatureType.setBounds(10, 28, 90, 14);
		panelOptions.add(lblSignatureType);

		cmbSignatureType = new JComboBox<String>();
		cmbSignatureType.setModel(new DefaultComboBoxModel<String>(new String[] {"Default", "BinaryCryptoAPI", "BinaryXML", "OpenXML", "OpenXPS", "OpenOffice"}));
		cmbSignatureType.setBounds(105, 25, 140, 22);
		panelOptions.add(cmbSignatureType);
		cmbSignatureType.setSelectedIndex(0);

		JLabel lblDigestMethod = new JLabel("Digest Method");
		lblDigestMethod.setBounds(290, 28, 100, 14);
		panelOptions.add(lblDigestMethod);

		cmbHashAlgorithm = new JComboBox<String>();
		cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"MD5", "SHA1", "SHA224", "SHA256", "SHA384", "SHA512", "RIPEMD160", "GOST", "Whirlpool"}));
		cmbHashAlgorithm.setBounds(385, 25, 140, 22);
		panelOptions.add(cmbHashAlgorithm);
		cmbHashAlgorithm.setSelectedIndex(3);

		JLabel lblSignCertFile = new JLabel("Signing certificate:");
		lblSignCertFile.setBounds(10, 63, 150, 14);
		panelOptions.add(lblSignCertFile);

		edSignCertFile = new JTextField();
		edSignCertFile.setBounds(10, 80, 240, 20);
		panelOptions.add(edSignCertFile);
		edSignCertFile.setColumns(10);

		JButton sbBrowseSignCertFile = new JButton("Browse");
		sbBrowseSignCertFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edSignCertFile.setText(getSaveFileName());
			}
		});
		sbBrowseSignCertFile.setBounds(255, 78, 80, 25);
		panelOptions.add(sbBrowseSignCertFile);

		JLabel lblSignCertPassword = new JLabel("Certificate password:");
		lblSignCertPassword.setBounds(10, 113, 130, 14);
		panelOptions.add(lblSignCertPassword);

		edSignCertPassword = new JTextField();
		edSignCertPassword.setBounds(140, 110, 195, 20);
		panelOptions.add(edSignCertPassword);
		edSignCertPassword.setColumns(10);

		JPanel panel_2 = new JPanel();
		panel_2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing Chain", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_2.setBounds(340, 60, 330, 100);
		panelOptions.add(panel_2);
		panel_2.setLayout(null);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 20, 230, 75);
		panel_2.add(scrollPane);

		tableSigningChain = new JTable();
		tableSigningChain.setModel(new DefaultTableModel(
				new Object[][] {
				},
				new String[] {
						"Serial", "Issuer"
				}
		));
		tableSigningChain.setFillsViewportHeight(true);
		scrollPane.setViewportView(tableSigningChain);

		btnAdd = new JButton("Add");
		btnAdd.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addClick();
			}
		});
		btnAdd.setBounds(245, 20, 80, 25);
		panel_2.add(btnAdd);

		btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeClick();
			}
		});
		btnRemove.setBounds(245, 55, 80, 25);
		panel_2.add(btnRemove);

		JPanel panelAddOptions = new JPanel();
		panelAddOptions.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Additional options  ", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panelAddOptions.setBounds(5, 260, 675, 140);
		contentPanel.add(panelAddOptions);
		panelAddOptions.setLayout(null);

		cbSignDocument = new JCheckBox("Sign Document");
		cbSignDocument.setBounds(5, 25, 120, 23);
		panelAddOptions.add(cbSignDocument);
		cbSignDocument.setSelected(true);

		cbSignSignatureOrigin = new JCheckBox("Sign Signature Origin");
		cbSignSignatureOrigin.setBounds(5, 55, 150, 23);
		panelAddOptions.add(cbSignSignatureOrigin);

		cbSignCoreProperties = new JCheckBox("Sign Document Properties");
		cbSignCoreProperties.setBounds(5, 85, 200, 23);
		panelAddOptions.add(cbSignCoreProperties);

		JPanel pane1 = new JPanel();
		pane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signature Info", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane1.setBounds(340, 20, 330, 110);
		panelAddOptions.add(pane1);
		pane1.setLayout(null);

		cbIncludeSigInfo = new JCheckBox("Include");
		cbIncludeSigInfo.setBounds(10, 20, 100, 23);
		pane1.add(cbIncludeSigInfo);

		JLabel lblSigText = new JLabel("Signature Text:");
		lblSigText.setBounds(10, 53, 90, 14);
		pane1.add(lblSigText);

		edSigText = new JTextField();
		edSigText.setBounds(110, 50, 210, 20);
		pane1.add(edSigText);
		edSigText.setColumns(10);

		JLabel lblSigComments = new JLabel("Signature Comments:");
		lblSigComments.setBounds(10, 83, 130, 14);
		pane1.add(lblSigComments);

		edSigComments = new JTextField();
		edSigComments.setBounds(145, 80, 175, 20);
		pane1.add(edSigComments);
		edSigComments.setColumns(10);

		JButton btnSign = new JButton("Sign");
		btnSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sign();
			}
		});
		btnSign.setBounds(597, 410, 80, 25);
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

	protected void addClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			signer.getSigningChain().add(cert);
			updateSigningChain();
		}
	}

	protected void removeClick()
	{
		signer.getSigningChain().remove(tableSigningChain.getSelectedRow());
		((DefaultTableModel)tableSigningChain.getModel()).removeRow(tableSigningChain.getSelectedRow());
	}

	private void addSigningCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)tableSigningChain.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearSigningChainTable() {
		((DefaultTableModel)tableSigningChain.getModel()).setNumRows(0);
	}

	private void updateSigningChain() {
		clearSigningChainTable();

		for (int i = 0; i < signer.getSigningChain().size(); i++)
		{
			String s = signer.getSigningChain().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addSigningCertItem(bytesToHex(signer.getSigningChain().item(i).getSerialNumber()), s);
		}
	}

	protected void sign()
	{
		try
		{
			signer.setInputFile(edInputFile.getText());
			signer.setOutputFile(edOutputFile.getText());

			signer.setSigningCertificate(LoadCertificate(edSignCertFile.getText(), edSignCertPassword.getText()));


			switch (cmbSignatureType.getSelectedIndex()) {
				case 0:
					signer.getNewSignature().setSignatureType(OfficeSignature.ostDefault);
					break;
				case 1:
					signer.getNewSignature().setSignatureType(OfficeSignature.ostBinaryCryptoAPI);
					break;
				case 2:
					signer.getNewSignature().setSignatureType(OfficeSignature.ostBinaryXML);
					break;
				case 3:
					signer.getNewSignature().setSignatureType(OfficeSignature.ostOpenXML);
					break;
				case 4:
					signer.getNewSignature().setSignatureType(OfficeSignature.ostOpenXPS);
					break;
				case 5:
					signer.getNewSignature().setSignatureType(OfficeSignature.ostOpenDocument);
					break;
			}

			signer.getNewSignature().setHashAlgorithm(cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));

			signer.getNewSignature().setDocumentSigned(cbSignDocument.isSelected());
			signer.getNewSignature().setCorePropertiesSigned(cbSignCoreProperties.isSelected());
			signer.getNewSignature().setSignatureOriginSigned(cbSignSignatureOrigin.isSelected());

			if (cbIncludeSigInfo.isSelected())
			{
				signer.config("SignatureInfoIncluded=true");
			}
			else
			{
				signer.config("SignatureInfoIncluded=false");
			}

			signer.config("SignatureInfoText=" + edSigText.getText());
			signer.config("SignatureInfoComments=" + edSigComments.getText());

			signer.sign();

			showMessage("Info", "Office file successfully signed");
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



