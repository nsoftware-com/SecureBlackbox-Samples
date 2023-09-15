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

import static secureblackbox.OfficeSignature.*;

public class validationresultdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JComboBox<String> cbSignatures;
	private java.util.ArrayList<OfficeSignature> objects;
	private JPanel pSignatureInfo;
	private JTextField tbSignatureType;
	private JLabel lIsDocumentSigned;
	private JLabel lIsCorePropertiesSigned;
	private JLabel lIsSignatureOriginSigned;
	private JLabel lSignatureTime;
	private JLabel lSignatureValidationResult;
	private JLabel lChainValidationResult;

	private Officeverifier verifier;

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
		setBounds(100, 100, 377, 400);
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
					OfficeSignature sig = objects.get(cbSignatures.getSelectedIndex());

					switch (sig.getSignatureType())
					{
						case ostBinaryCryptoAPI: tbSignatureType.setText("BinaryCryptoAPI"); break;
						case ostBinaryXML: tbSignatureType.setText("BinaryXML"); break;
						case ostOpenXML: tbSignatureType.setText("OpenXML"); break;
						case ostOpenXPS: tbSignatureType.setText("OpenXPS"); break;
						case ostOpenDocument: tbSignatureType.setText("OpenDocument"); break;
						default: tbSignatureType.setText("Unknown"); break;
					}

					if (sig.getDocumentSigned())
						lIsDocumentSigned.setText("Document content is signed");
					else
						lIsDocumentSigned.setText("Document content is partially signed");

					if (sig.getCorePropertiesSigned())
						lIsCorePropertiesSigned.setText("Document properties are signed");
					else
						lIsCorePropertiesSigned.setText("Document properties are not signed");

					if (sig.getSignatureOriginSigned())
						lIsSignatureOriginSigned.setText("Signature origin is signed");
					else
						lIsSignatureOriginSigned.setText("Signature origin is not signed");

					lSignatureTime.setText("Signature Time: " + sig.getClaimedSigningTime());

					String s = "";
					switch (sig.getSignatureValidationResult())
					{
						case OfficeSignature.svtValid: s = "Valid"; break;
						case OfficeSignature.svtCorrupted: s = "Corrupted"; break;
						case OfficeSignature.svtSignerNotFound: s = "SignerNotFound"; break;
						case OfficeSignature.svtFailure: s = "Failure"; break;
						default: s = "Unknown"; break;
					}

					lSignatureValidationResult.setText("Signature Validation Result: " + s);
					s = "";
					switch (sig.getChainValidationResult())
					{
						case OfficeSignature.cvtValid: s = "Valid"; break;
						case OfficeSignature.cvtValidButUntrusted: s = "ValidButUntrusted"; break;
						case OfficeSignature.cvtInvalid: s = "Invalid"; break;
						case OfficeSignature.cvtCantBeEstablished: s = "CantBeEstablished"; break;
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
		pSignatureInfo.setBounds(10, 65, 349, 250);
		contentPanel.add(pSignatureInfo);
		pSignatureInfo.setLayout(null);

		JLabel label = new JLabel("Signature Type");
		label.setBounds(0, 0, 217, 14);
		pSignatureInfo.add(label);

		tbSignatureType = new JTextField();
		tbSignatureType.setEditable(false);
		tbSignatureType.setColumns(10);
		tbSignatureType.setBounds(0, 18, 348, 20);
		pSignatureInfo.add(tbSignatureType);

		lIsDocumentSigned = new JLabel("IsDocumentSigned");
		lIsDocumentSigned.setBounds(0, 50, 300, 14);
		pSignatureInfo.add(lIsDocumentSigned);

		lIsCorePropertiesSigned = new JLabel("IsCorePropertiesSigned");
		lIsCorePropertiesSigned.setBounds(0, 70, 300, 14);
		pSignatureInfo.add(lIsCorePropertiesSigned);

		lIsSignatureOriginSigned = new JLabel("IsSignatureOriginSigned");
		lIsSignatureOriginSigned.setBounds(0, 90, 300, 14);
		pSignatureInfo.add(lIsSignatureOriginSigned);

		lSignatureTime = new JLabel("Signature Time");
		lSignatureTime.setBounds(0, 120, 300, 14);
		pSignatureInfo.add(lSignatureTime);

		lSignatureValidationResult = new JLabel("SignatureValidationResult");
		lSignatureValidationResult.setBounds(0, 160, 348, 14);
		pSignatureInfo.add(lSignatureValidationResult);

		lChainValidationResult = new JLabel("ChainValidationResult");
		lChainValidationResult.setBounds(0, 185, 348, 14);
		pSignatureInfo.add(lChainValidationResult);

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

		objects = new java.util.ArrayList<OfficeSignature>();
	}

	public void Init(Officeverifier _verifier) {
		verifier = _verifier;
		cbSignatures.removeAll();
		objects.clear();
		tbSignatureType.setText("");
		pSignatureInfo.setVisible(false);
		for (int i = 0; i < verifier.getSignatures().size(); i++)
		{
			objects.add(verifier.getSignatures().item(i));
			cbSignatures.addItem("Signature " + (i + 1));
		}
	}
}
