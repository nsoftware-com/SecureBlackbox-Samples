import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

public class signaturesdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable table;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			signaturesdialog dialog = new signaturesdialog(null, null);
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public signaturesdialog(PGPSignatureList signatures, Pgpkeyring keyring) {
		setTitle("Signatures");
		setBounds(100, 100, 530, 300);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JScrollPane scrollPane = new JScrollPane();
			scrollPane.setBounds(5, 10, 505, 210);
			contentPanel.add(scrollPane);
			{
				table = new JTable();
				table.setModel(new  DefaultTableModel(
						new Object[][] {
						},
						new String[] {
								"Signer", "Validity"
						}
				));
				table.setFillsViewportHeight(true);
				scrollPane.setViewportView(table);
			}
		}
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				JButton cancelButton = new JButton("Close");
				cancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						setVisible(false);
					}
				});
				cancelButton.setActionCommand("Cancel");
				buttonPane.add(cancelButton);
			}
		}

		clearTable();
		for (int i = 0; i < signatures.size(); i++)
		{
			String userID = "Unknown Key";
			for (int j = 0; j < keyring.getPublicKeys().size(); j++)
			{
				if (!keyring.getPublicKeys().item(j).getIsSubkey() && keyring.getPublicKeys().item(j).getKeyID().equalsIgnoreCase(signatures.item(i).getSignerKeyID()))
				{
					if (keyring.getPublicKeys().item(j).getUsername() == "")
						userID = "No name";
					else
						userID = keyring.getPublicKeys().item(j).getUsername();
					break;
				}
			}

			String sigVal;
			switch(signatures.item(i).getValidity())
			{
				case PGPSignature.psvCorrupted:
					sigVal = "Corrupted";
					break;
				case PGPSignature.psvNoKey:
					sigVal = "Signing key not found, unable to verify";
					break;
				case PGPSignature.psvUnknownAlgorithm:
					sigVal = "Unknown signing algorithm";
					break;
				case PGPSignature.psvValid:
					sigVal = "Valid";
					break;
				default:
					sigVal = "Unknown reason";
					break;
			}

			addRow(userID, sigVal);
		}
	}

	private void addRow(String signer, String validity) {
		DefaultTableModel model = (DefaultTableModel)table.getModel();
		model.insertRow(model.getRowCount(), new Object[] {signer, validity});
	}

	private void clearTable() {
		((DefaultTableModel)table.getModel()).setNumRows(0);
	}
}
