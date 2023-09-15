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
import java.io.File;
import java.awt.event.ActionEvent;

import secureblackbox.*;

import static secureblackbox.Xmlsigner.*;

class xmlsigner extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;

	private JCheckBox cbDetached;
	private JComboBox<String> cmbCanonMethod;
	private JComboBox<String> cmbHashAlgorithm;
	private JButton btnReferences;
	private JCheckBox cbIncludeKey;
	private JTextField edKeyName;
	
	private JTextField edSignCertFile;
	private JTextField edSignCertPassword;
	private JTable tableCerts;
	private JButton btnAddCerts;
	private JButton btnRemoveCerts;

	Xmlsigner signer;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			xmlsigner dialog = new xmlsigner();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public xmlsigner()
	{
		signer = new Xmlsigner();
		try
		{
			signer.addXmlsignerEventListener(new XmlsignerEventListener() {
				
				@Override
				public void supercoreIntercept(XmlsignerSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void resolveReference(XmlsignerResolveReferenceEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void formatText(XmlsignerFormatTextEvent e) {
					// TODO Auto-generated method stub
				}
				
				@Override
				public void formatElement(XmlsignerFormatElementEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void externalSign(XmlsignerExternalSignEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void error(XmlsignerErrorEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void notification(XmlsignerNotificationEvent e) {
					// TODO Auto-generated method stub
				
				}
			});
		}
		catch (Exception ex)
		{}

		setTitle("XML Signer");

		setBounds(100, 100, 740, 470);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample shows how to create XAdES signatures. Please select an input file, tune up the signing options, and click 'Sign'.");
		lblCaption.setBounds(10, 5, 730, 14);
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

		JPanel panel = new JPanel();
		panel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "General", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel.setBounds(10, 95, 350, 130);
		contentPanel.add(panel);
		panel.setLayout(null);

		cbDetached = new JCheckBox("Detached");
		cbDetached.setSelected(false);
		cbDetached.setBounds(10, 25, 166, 23);
		panel.add(cbDetached);

		JLabel lblCanonicalizationMethod = new JLabel("Canonicalization method");
		lblCanonicalizationMethod.setBounds(10, 63, 144, 14);
		panel.add(lblCanonicalizationMethod);

		cmbCanonMethod = new JComboBox<String>();
		cmbCanonMethod.setModel(new DefaultComboBoxModel<String>(new String[] {"Canonical", "Canonical with comments", "Canonical v1.1", "Canonical with comments v1.1", "Exclusive canonical", "Exclusive canonical with comments", "Minimal canonical"}));
		cmbCanonMethod.setBounds(155, 60, 180, 22);
		panel.add(cmbCanonMethod);

		JLabel lbHashAlgorithm = new JLabel("Hash algorithm");
		lbHashAlgorithm.setBounds(10, 99, 107, 14);
		panel.add(lbHashAlgorithm);

		cmbHashAlgorithm = new JComboBox<String>();
		cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"SHA1", "MD5", "SHA256", "SHA384", "SHA512", "RIPEMD160"}));
		cmbHashAlgorithm.setBounds(155, 95, 100, 22);
		panel.add(cmbHashAlgorithm);

		JPanel Signpane1 = new JPanel();
		Signpane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing options", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Signpane1.setBounds(10, 230, 710, 165);
		contentPanel.add(Signpane1);
		Signpane1.setLayout(null);
		
		cbIncludeKey = new JCheckBox("Include Key (public part)");
		cbIncludeKey.setSelected(true);
		cbIncludeKey.setBounds(10, 20, 166, 23);
		Signpane1.add(cbIncludeKey);

		JLabel lblKeyName = new JLabel("Key Name");
		lblKeyName.setBounds(250, 23, 81, 14);
		Signpane1.add(lblKeyName);

		edKeyName = new JTextField();
		edKeyName.setBounds(320, 20, 170, 20);
		Signpane1.add(edKeyName);
		edKeyName.setColumns(10);

		JLabel lblSignCertFile = new JLabel("Signing certificate:");
		lblSignCertFile.setBounds(10, 55, 150, 14);
		Signpane1.add(lblSignCertFile);

		edSignCertFile = new JTextField();
		edSignCertFile.setBounds(10, 75, 240, 20);
		edSignCertFile.setColumns(10);
		Signpane1.add(edSignCertFile);

		JButton sbBrowseSignCertFile = new JButton("Browse");
		sbBrowseSignCertFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edSignCertFile.setText(getSaveFileName());
			}
		});
		sbBrowseSignCertFile.setBounds(260, 73, 80, 25);
		Signpane1.add(sbBrowseSignCertFile);

		JLabel lblSignCertPassword = new JLabel("Certificate password:");
		lblSignCertPassword.setBounds(10, 108, 130, 14);
		Signpane1.add(lblSignCertPassword);

		edSignCertPassword = new JTextField();
		edSignCertPassword.setBounds(140, 105, 200, 20);
		edSignCertPassword.setColumns(10);
		Signpane1.add(edSignCertPassword);

		JPanel pane1 = new JPanel();
		pane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing Chain", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		pane1.setBounds(360, 50, 345, 110);
		Signpane1.add(pane1);
		pane1.setLayout(null);

		JScrollPane scrollPanelChain = new JScrollPane();
		scrollPanelChain.setBounds(5, 20, 250, 85);
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
		btnAddCerts.setBounds(260, 20, 80, 25);
		pane1.add(btnAddCerts);

		btnRemoveCerts = new JButton("Remove");
		btnRemoveCerts.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeCertsClick();
			}
		});
		btnRemoveCerts.setBounds(260, 55, 80, 25);
		pane1.add(btnRemoveCerts);

		btnReferences = new JButton("References");
		btnReferences.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showReferencesDialog();
			}
		});
		btnReferences.setBounds(10, 400, 100, 25);
		contentPanel.add(btnReferences);

		JButton btnSign = new JButton("Sign");
		btnSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sign();
			}
		});
		btnSign.setBounds(638, 400, 80, 25);
		contentPanel.add(btnSign);

		cmbCanonMethod.setSelectedIndex(0);
		cmbHashAlgorithm.setSelectedIndex(0);
	}

	protected void sign()
	{
		try
		{
			signer.setOutputFile(edOutputFile.getText());

			if (cbDetached.isSelected()) {
				signer.setDataFile(edInputFile.getText());
				signer.setDataType(Xmlverifier.cxdtBinary);
				File f = new File(edInputFile.getText());
				signer.setDataURI(f.getName());

                signer.setSignatureType(XMLSignature.cxstDetached);
            }
            else {
				signer.setInputFile(edInputFile.getText());
				signer.setSignatureType(XMLSignature.cxstEnveloped);
			}
			
			signer.setSigningCertificate(LoadCertificate(edSignCertFile.getText(), edSignCertPassword.getText()));

			switch (cmbCanonMethod.getSelectedIndex()) {
				case 0: {
					signer.setCanonicalizationMethod(cxcmCanon);
					break;
				}
				case 1: {
					signer.setCanonicalizationMethod(cxcmCanonComment);
					break;
				}
				case 2: {
					signer.setCanonicalizationMethod(cxcmCanon_v1_1);
					break;
				}
				case 3: {
					signer.setCanonicalizationMethod(cxcmCanonComment_v1_1);
					break;
				}
				case 4: {
					signer.setCanonicalizationMethod(cxcmExclCanon);
					break;
				}
				case 5: {
					signer.setCanonicalizationMethod(cxcmExclCanonComment);
					break;
				}
				case 6: {
					signer.setCanonicalizationMethod(cxcmMinCanon);
					break;
				}
				default: {
					signer.setCanonicalizationMethod(cxcmCanon);
					break;
				}
			}

			signer.setHashAlgorithm(cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));

			signer.config("KeyName=" + edKeyName.getText());
			if (cbIncludeKey.isSelected())
				signer.config("IncludeKey=true");
			else
				signer.config("IncludeKey=false");

			// Enable automatic signature formatting 
			signer.config("XMLFormatting=auto");

			signer.sign();

			showMessage("Info", "XML file successfully signed");
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

	String createString(char n, int len) {
		StringBuilder sb = new StringBuilder(len);
		while (len-- > 0)
			sb.append(n);
		
		return sb.toString();
	}

	public void showReferencesDialog()
	{
		refsdialog frmReferences = new refsdialog(signer.getReferences());
		frmReferences.setLocationRelativeTo(this);
		frmReferences.setModal(true);
		frmReferences.setVisible(true);

		frmReferences.dispose();
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

	protected void addCertsClick()
	{
		String fileName = getFileName();

		if (fileName.length() > 0)
		{
			Certificate cert = LoadCertificate(fileName, requestPassword());
			signer.getSigningChain().add(cert);
			updateCertificates();
		}
	}

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	private void addSignCertItem(String serialNumber, String issuer) {
		DefaultTableModel model = (DefaultTableModel)tableCerts.getModel();
		model.insertRow(model.getRowCount(), new Object[] {serialNumber, issuer});
	}

	private void clearSigningCertificatesTable() {
		((DefaultTableModel)tableCerts.getModel()).setNumRows(0);
	}

	private void updateCertificates() {
		clearSigningCertificatesTable();

		for (int i = 0; i < signer.getSigningChain().size(); i++)
		{
			String s = signer.getSigningChain().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addSignCertItem(bytesToHex(signer.getSigningChain().item(i).getSerialNumber()), s);
		}
	}

	protected void removeCertsClick() {
		((DefaultTableModel)tableCerts.getModel()).removeRow(tableCerts.getSelectedRow());
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



