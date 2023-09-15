import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import secureblackbox.*;

public class validationresultdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JComboBox<String> cbSignatures;
	private java.util.ArrayList<JAdESSignature> objects;
	private JPanel pSignatureInfo;
	private JLabel lTimestamp;
	private JLabel lSignatureValidationResult;
	private JLabel lChainValidationResult;

	private Jadesverifier verifier;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			validationresultdialog dialog = new validationresultdialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public validationresultdialog() {
		setTitle("Validation results");
		setBounds(100, 100, 377, 260);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JLabel lblTheDocumentContains = new JLabel("The document contains the following digital signatures");
			lblTheDocumentContains.setBounds(10, 11, 394, 14);
			contentPanel.add(lblTheDocumentContains);
		}

		cbSignatures = new JComboBox<String>();
		cbSignatures.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				if (cbSignatures.getSelectedIndex() != -1) {
					JAdESSignature sig = objects.get(cbSignatures.getSelectedIndex());
					lTimestamp.setText("Claimed Signing Time: " + sig.getClaimedSigningTime() + " UTC");
					String s = "";
					switch (sig.getSignatureValidationResult()) {
						case JAdESSignature.svtValid:
							s = "Valid";
							break;
						case JAdESSignature.svtCorrupted:
							s = "Corrupted";
							break;
						case JAdESSignature.svtSignerNotFound:
							s = "SignerNotFound";
							break;
						case JAdESSignature.svtFailure:
							s = "Failure";
							break;
						default:
							s = "Unknown";
							break;
					}

					lSignatureValidationResult.setText("Signature Validation Result: " + s);

					s = "";
					switch (sig.getChainValidationResult()) {
						case JAdESSignature.cvtValid:
							s = "Valid";
							break;
						case JAdESSignature.cvtValidButUntrusted:
							s = "ValidButUntrusted";
							break;
						case JAdESSignature.cvtInvalid:
							s = "Invalid";
							break;
						case JAdESSignature.cvtCantBeEstablished:
							s = "CantBeEstablished";
							break;
					}

					lChainValidationResult.setText("Chain Validation Result: " + s);

					pSignatureInfo.setVisible(true);
				} else
					pSignatureInfo.setVisible(false);

			}
		});
		cbSignatures.setBounds(10, 32, 348, 22);
		contentPanel.add(cbSignatures);

		pSignatureInfo = new JPanel();
		pSignatureInfo.setBounds(10, 65, 349, 109);
		contentPanel.add(pSignatureInfo);
		pSignatureInfo.setLayout(null);

		lTimestamp = new JLabel("Timestamp");
		lTimestamp.setBounds(0, 0, 348, 14);
		pSignatureInfo.add(lTimestamp);

		lSignatureValidationResult = new JLabel("SignatureValidationResult");
		lSignatureValidationResult.setBounds(0, 25, 348, 14);
		pSignatureInfo.add(lSignatureValidationResult);

		lChainValidationResult = new JLabel("ChainValidationResult");
		lChainValidationResult.setBounds(0, 50, 348, 14);
		pSignatureInfo.add(lChainValidationResult);

		JPanel buttonPane = new JPanel();
		buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
		getContentPane().add(buttonPane, BorderLayout.SOUTH);
		{
			JButton okButton = new JButton("Close");
			okButton.setActionCommand("Close");
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					setVisible(false);
				}
			});
			buttonPane.add(okButton);
			getRootPane().setDefaultButton(okButton);
		}

		objects = new java.util.ArrayList<JAdESSignature>();
	}

	private String getSaveFileName() {
		JFileChooser fc = new JFileChooser();

		FileNameExtensionFilter filter = new FileNameExtensionFilter("JSON files", "json");
		fc.setFileFilter(filter);

		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	public void Init(Jadesverifier _verifier) {
		verifier = _verifier;
		cbSignatures.removeAll();
		objects.clear();
		pSignatureInfo.setVisible(false);
		for (int i = 0; i < verifier.getSignatures().size(); i++)
		{
			objects.add(verifier.getSignatures().item(i));
			cbSignatures.addItem("Signature #" + (i + 1));
		}
	}
}
