import java.awt.*;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

import static secureblackbox.Xadesverifier.*;

class signdialog extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();

	private JTextField edCanonMethod;
	private JTextField edHashAlgorithm;
	private JTextField edKeyName;
	private JTable table;
	private JButton btnAdd;
	private JButton btnRemove;
	protected boolean isOk = false;

	Xadesverifier verifier;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			signdialog dialog = new signdialog(null, 0);
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public signdialog(Xadesverifier _verifier, Integer sigIndex)
	{
		setTitle("Signature Options");

		setBounds(100, 100, 385, 390);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JPanel panel = new JPanel();
		panel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "General", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel.setBounds(5, 10, 360, 120);
		contentPanel.add(panel);
		panel.setLayout(null);

		JLabel lblCanonicalizationMethod = new JLabel("Canonicalization method");
		lblCanonicalizationMethod.setBounds(10, 23, 144, 14);
		panel.add(lblCanonicalizationMethod);

		edCanonMethod = new JTextField();
		edCanonMethod.setBounds(164, 20, 180, 22);
		edCanonMethod.setEnabled(false);
		panel.add(edCanonMethod);

		JLabel lbSignatureMethod = new JLabel("Hash algorithm");
		lbSignatureMethod.setBounds(10, 53, 107, 14);
		panel.add(lbSignatureMethod);

		edHashAlgorithm = new JTextField();
		edHashAlgorithm.setBounds(164, 50, 180, 22);
		edHashAlgorithm.setEnabled(false);
		panel.add(edHashAlgorithm);

		JPanel panel_3 = new JPanel();
		panel_3.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Key Info", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_3.setBounds(5, 140, 360, 170);
		contentPanel.add(panel_3);
		panel_3.setLayout(null);

		JLabel lblKeyName = new JLabel("Key Name");
		lblKeyName.setBounds(15, 22, 81, 14);
		panel_3.add(lblKeyName);

		edKeyName = new JTextField();
		edKeyName.setBounds(101, 20, 170, 20);
		edKeyName.setEnabled(false);
		panel_3.add(edKeyName);
		edKeyName.setColumns(10);

		JPanel panel_2 = new JPanel();
		panel_2.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Known Certificate's", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		panel_2.setBounds(5, 45, 350, 120);
		panel_3.add(panel_2);
		panel_2.setLayout(null);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(5, 20, 250, 90);
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
		btnAdd.setBounds(260, 20, 80, 25);
		panel_2.add(btnAdd);

		btnRemove = new JButton("Remove");
		btnRemove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeClick();
			}
		});
		btnRemove.setBounds(260, 55, 80, 25);
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
		if (verifier != null)
		{
			XAdESSignature sig = verifier.getSignatures().get(sigIndex);
			switch (sig.getCanonicalizationMethod()) {
				case XAdESSignature.cxcmCanon: {
					edCanonMethod.setText("Canonical");
					break;
				}
				case XAdESSignature.cxcmCanonComment: {
					edCanonMethod.setText("Canonical with comments");
					break;
				}
				case XAdESSignature.cxcmCanon_v1_1: {
					edCanonMethod.setText("Canonical v1.1");
					break;
				}
				case XAdESSignature.cxcmCanonComment_v1_1: {
					edCanonMethod.setText("Canonical with comments v1.1");
					break;
				}
				case XAdESSignature.cxcmExclCanon: {
					edCanonMethod.setText("Exclusive canonical");
					break;
				}
				case XAdESSignature.cxcmExclCanonComment: {
					edCanonMethod.setText("Exclusive canonical with comments");
					break;
				}
				case XAdESSignature.cxcmMinCanon: {
					edCanonMethod.setText("Minimal canonical");
					break;
				}
				default: {
					edCanonMethod.setText("Unknown");
					break;
				}
			}

			edHashAlgorithm.setText(sig.getHashAlgorithm());

			try
			{
				edKeyName.setText(verifier.config("KeyName"));
			}
			catch (Exception e)
			{}
		}
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

	private static String bytesToHex(byte[] hashInBytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : hashInBytes) {
			sb.append(String.format("%02x", b));
		}
		return sb.toString();
	}

	private void addSignCertItem(String serialNumber, String issuer) {
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

			addSignCertItem(bytesToHex(verifier.getKnownCertificates().item(i).getSerialNumber()), s);
		}
	}

	protected void removeClick() {
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