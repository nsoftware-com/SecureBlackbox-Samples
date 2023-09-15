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

class cadessigner extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edInputFile;
	private JTextField edOutputFile;

	private JComboBox<String> cmbLevel;
	private JComboBox<String> cmbHashAlgorithm;
	private JCheckBox cbDetached;

	private JTextField edSignCertFile;
	private JTextField edSignCertPassword;
	private JTable tableCerts;
	private JButton btnAddCerts;
	private JButton btnRemoveCerts;

	private JCheckBox cbTimestamp;
	private JTextField edTimestampServer;

	Cadessigner signer;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			cadessigner dialog = new cadessigner();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public cadessigner()
	{
		signer = new Cadessigner();

		setTitle("CAdES Signer demo");

		setBounds(100, 100, 880, 435);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblCaption = new JLabel("This sample shows how to create CAdES signatures. Please select an input file, tune up the signing options, and click 'Sign'.");
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

		JLabel lblOutputPath = new JLabel("Output File");
		lblOutputPath.setBounds(10, 63, 70, 14);
		contentPanel.add(lblOutputPath);

		edOutputFile = new JTextField();
		edOutputFile.setBounds(80, 60, 440, 20);
		contentPanel.add(edOutputFile);
		edOutputFile.setColumns(10);

		JButton sbBrowseOutputFile = new JButton("Browse");
		sbBrowseOutputFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edOutputFile.setText(getSaveFileName());
			}
		});
		sbBrowseOutputFile.setBounds(525, 58, 80, 25);
		contentPanel.add(sbBrowseOutputFile);
		contentPanel.setLayout(null);

		JPanel Signpane1 = new JPanel();
		Signpane1.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Signing options", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		Signpane1.setBounds(10, 100, 845, 165);
		contentPanel.add(Signpane1);
		Signpane1.setLayout(null);

		JLabel lblLevel = new JLabel("Level");
		lblLevel.setBounds(10, 23, 40, 14);
		Signpane1.add(lblLevel);

		cmbLevel = new JComboBox<String>();
		cmbLevel.setModel(new DefaultComboBoxModel<String>(new String[] {"BES", "EPES", "T", "C", "XType1", "XType2", "XLType1", "XLType2", "BaselineB", "BaselineT", "BaselineLT", "BaselineLTA", "ExtendedBES", "ExtendedEPES", "ExtendedT", "ExtendedC", "ExtendedXType1", "ExtendedXType2", "ExtendedXLType1", "ExtendedXLType2", "A", "ExtendedA"}));
		cmbLevel.setBounds(55, 20, 140, 22);
		Signpane1.add(cmbLevel);

		JLabel lblHashAlgorithm = new JLabel("Hash algorithm");
		lblHashAlgorithm.setBounds(220, 23, 90, 14);
		Signpane1.add(lblHashAlgorithm);

		cmbHashAlgorithm = new JComboBox<String>();
		cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"SHA1", "MD5", "SHA256", "SHA384", "SHA512", "RIPEMD160"}));
		cmbHashAlgorithm.setBounds(325, 20, 90, 22);
		Signpane1.add(cmbHashAlgorithm);

		cbDetached = new JCheckBox("Detached");
		cbDetached.setBounds(470, 19, 250, 23);
		Signpane1.add(cbDetached);

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
		Additianalpane1.setBounds(10, 270, 845, 80);
		contentPanel.add(Additianalpane1);
		Additianalpane1.setLayout(null);

		cbTimestamp = new JCheckBox("Request a timestamp from TSA server:");
		cbTimestamp.setBounds(10, 20, 250, 23);
		Additianalpane1.add(cbTimestamp);

		edTimestampServer = new JTextField();
		edTimestampServer.setText("http://");
		edTimestampServer.setBounds(25, 50, 370, 20);
		edTimestampServer.setColumns(10);
		Additianalpane1.add(edTimestampServer);

		JButton btnSign = new JButton("Sign");
		btnSign.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sign();
			}
		});
		btnSign.setBounds(770, 360, 80, 25);
		contentPanel.add(btnSign);
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

	protected void sign()
	{
		try
		{
			signer.setInputFile(edInputFile.getText());
			signer.setOutputFile(edOutputFile.getText());

			signer.setHashAlgorithm(cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));

			signer.setSigningCertificate(LoadCertificate(edSignCertFile.getText(), edSignCertPassword.getText()));

			if (cbTimestamp.isSelected())
			{
				signer.setTimestampServer(edTimestampServer.getText());
			}
			else
			{
				signer.setTimestampServer("");
			}

			signer.config("IgnoreChainValidationErrors=true");

			signer.sign(cmbLevel.getSelectedIndex() + 1, cbDetached.isSelected());

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



