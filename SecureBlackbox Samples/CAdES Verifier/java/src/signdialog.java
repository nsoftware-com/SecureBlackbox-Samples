import java.awt.*;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

class signdialog extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();

	private JTextField edSignatureName;
	private JTextField edIssuerRDN;
	private JTextField edSerialNumber;
	private JTextField edSubjectKeyID;
	private JTable table;
	private JButton btnAdd;
	private JButton btnRemove;
	protected boolean isOk = false;

	Cadesverifier verifier;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			signdialog dialog = new signdialog(null, "", null, null);
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
	public signdialog(Cadesverifier _verifier, String issuerRDN, byte[] serialNumber, byte[] subjectKeyID)
	{
		setTitle("Signature Options");

		setBounds(100, 100, 365, 305);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblIssuerRDN = new JLabel("Issuer RDN:");
		lblIssuerRDN.setBounds(10, 23, 90, 14);
		contentPanel.add(lblIssuerRDN);

		edIssuerRDN = new JTextField();
		edIssuerRDN.setBounds(110, 20, 230, 22);
		edIssuerRDN.setEnabled(false);
		contentPanel.add(edIssuerRDN);

		JLabel lblSerialNumber = new JLabel("Serial Number:");
		lblSerialNumber.setBounds(10, 53, 90, 14);
		contentPanel.add(lblSerialNumber);

		edSerialNumber = new JTextField();
		edSerialNumber.setBounds(110, 50, 230, 22);
		edSerialNumber.setEnabled(false);
		contentPanel.add(edSerialNumber);

		JLabel lblSubjectKeyID = new JLabel("Subject KeyID:");
		lblSubjectKeyID.setBounds(10, 83, 90, 14);
		contentPanel.add(lblSubjectKeyID);

		edSubjectKeyID = new JTextField();
		edSubjectKeyID.setBounds(110, 80, 230, 22);
		edSubjectKeyID.setEnabled(false);
		contentPanel.add(edSubjectKeyID);

		JPanel panel_2 = new JPanel();
		panel_2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_2.setBounds(5, 105, 340, 120);
		contentPanel.add(panel_2);
		panel_2.setLayout(null);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 20, 240, 90);
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

		btnAdd = new JButton("Add");
		btnAdd.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addClick();
			}
		});
		btnAdd.setBounds(250, 20, 80, 25);
		panel_2.add(btnAdd);

		btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeClick();
			}
		});
		btnRemove.setBounds(250, 55, 80, 25);
		panel_2.add(btnRemove);

		JPanel buttonPane = new JPanel();
		buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
		getContentPane().add(buttonPane, BorderLayout.SOUTH);

		JButton okButton = new JButton("OK");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				isOk = true;
				setVisible(false);
			}
		});
		okButton.setActionCommand("OK");
		buttonPane.add(okButton);
		getRootPane().setDefaultButton(okButton);

		JButton cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
			}
		});
		cancelButton.setActionCommand("Cancel");
		buttonPane.add(cancelButton);

		verifier = _verifier;
		edIssuerRDN.setText(issuerRDN);
		edSerialNumber.setText(bytesToHex(serialNumber));
		edSubjectKeyID.setText(bytesToHex(subjectKeyID));
		updateCertificates();
	}

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
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

	private void clearCertificatesTable() {
		((DefaultTableModel)table.getModel()).setNumRows(0);
	}

	private void updateCertificates() {
		clearCertificatesTable();

		for (int i = 0; i < verifier.getKnownCertificates().size(); i++)
		{
			String s = verifier.getKnownCertificates().item(i).getIssuer();
			if (s == "")
				s = "<unknown>";

			addSignCertItem(bytesToHex(verifier.getKnownCertificates().item(i).getSerialNumber()), s);
		}
	}

	protected void removeClick()
	{
		verifier.getKnownCertificates().remove(table.getSelectedRow());
		((DefaultTableModel)table.getModel()).removeRow(table.getSelectedRow());
	}

	String getFileName(){
		JFileChooser fc = new JFileChooser();

		int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	public boolean isOK() {
		return isOk;
	}
}