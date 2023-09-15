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
	private java.util.ArrayList<PDFSignature> objects;
	private JTextField tbAuthorName;
	private JTextField tbReason;
	private JPanel pSignatureInfo;
	private JLabel lTimestamp;
	private JLabel lSignatureValidationResult;
	private JLabel lChainValidationResult;

	private Pdfverifier verifier;

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
		setBounds(100, 100, 377, 360);
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
				if (cbSignatures.getSelectedIndex() != -1)
				{
					PDFSignature sig = objects.get(cbSignatures.getSelectedIndex());
					if (sig.getAuthorName().length() > 0)
						tbAuthorName.setText(sig.getAuthorName());
					else
						tbAuthorName.setText("<not specified>");

					if (sig.getReason().length() > 0)
						tbReason.setText(sig.getReason());
					else
						tbReason.setText("<not specified>");

					lTimestamp.setText("Timestamp: " + sig.getClaimedSigningTime() + " (local)");
					String s = "";
					switch (sig.getSignatureValidationResult())
					{
						case PDFSignature.svtValid: s = "Valid"; break;
						case PDFSignature.svtCorrupted: s = "Corrupted"; break;
						case PDFSignature.svtSignerNotFound: s = "SignerNotFound"; break;
						case PDFSignature.svtFailure: s = "Failure"; break;
						default: s = "Unknown"; break;
					}

					lSignatureValidationResult.setText("Signature Validation Result: " + s);
					s = "";
					switch (sig.getChainValidationResult())
					{
						case PDFSignature.cvtValid: s = "Valid"; break;
						case PDFSignature.cvtValidButUntrusted: s = "ValidButUntrusted"; break;
						case PDFSignature.cvtInvalid: s = "Invalid"; break;
						case PDFSignature.cvtCantBeEstablished: s = "CantBeEstablished"; break;
						default: s = "Unknown"; break;
					}

					lChainValidationResult.setText("Chain Validation Result: " + s);
					pSignatureInfo.setVisible(true);
				}
				else
					pSignatureInfo.setVisible(false);

			}
		});
		cbSignatures.setBounds(10, 32, 348, 22);
		contentPanel.add(cbSignatures);

		pSignatureInfo = new JPanel();
		pSignatureInfo.setBounds(10, 65, 349, 209);
		contentPanel.add(pSignatureInfo);
		pSignatureInfo.setLayout(null);

		JLabel label = new JLabel("Author's name");
		label.setBounds(0, 0, 217, 14);
		pSignatureInfo.add(label);

		tbAuthorName = new JTextField();
		tbAuthorName.setEditable(false);
		tbAuthorName.setColumns(10);
		tbAuthorName.setBounds(0, 18, 348, 20);
		pSignatureInfo.add(tbAuthorName);

		JLabel label_1 = new JLabel("Reason for signing");
		label_1.setBounds(0, 49, 204, 14);
		pSignatureInfo.add(label_1);

		tbReason = new JTextField();
		tbReason.setEditable(false);
		tbReason.setColumns(10);
		tbReason.setBounds(0, 69, 348, 20);
		pSignatureInfo.add(tbReason);

		lTimestamp = new JLabel("Timestamp");
		lTimestamp.setBounds(0, 100, 348, 14);
		pSignatureInfo.add(lTimestamp);

		lSignatureValidationResult = new JLabel("SignatureValidationResult");
		lSignatureValidationResult.setBounds(0, 125, 348, 14);
		pSignatureInfo.add(lSignatureValidationResult);

		lChainValidationResult = new JLabel("ChainValidationResult");
		lChainValidationResult.setBounds(0, 150, 348, 14);
		pSignatureInfo.add(lChainValidationResult);


		JButton btnExtract = new JButton("Extract signed version");
		btnExtract.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				exClick();
			}
		});
		btnExtract.setBounds(175, 185, 170, 23);
		pSignatureInfo.add(btnExtract);
		{
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
		}

		objects = new java.util.ArrayList<PDFSignature>();
	}

	private String getSaveFileName() {
		JFileChooser fc = new JFileChooser();

		FileNameExtensionFilter filter = new FileNameExtensionFilter("PDF files", "pdf");
		fc.setFileFilter(filter);

		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	protected void exClick()
	{
		if (cbSignatures.getSelectedIndex() == -1) return;
		String fileName = getSaveFileName();
		if (fileName.length() > 0)
		{
			try
			{
				verifier.setOutputFile(fileName);
				verifier.getSignedVersion(cbSignatures.getSelectedIndex());
			}
			catch (Exception ex) {
				showErrorMessage(ex.getMessage());
			}
		}
	}

	static void showErrorMessage(String msg){
		JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
	}

	public void Init(Pdfverifier _verifier) {
		verifier = _verifier;
		cbSignatures.removeAll();
		objects.clear();
		tbAuthorName.setText("");
		tbReason.setText("");
		pSignatureInfo.setVisible(false);
		for (int i = 0; i < verifier.getSignatures().size(); i++)
		{
			objects.add(verifier.getSignatures().item(i));
			cbSignatures.addItem(verifier.getSignatures().item(i).getSignatureName());
		}
	}
}
