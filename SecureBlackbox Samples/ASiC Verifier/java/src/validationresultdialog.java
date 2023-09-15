import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import secureblackbox.*;

public class validationresultdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JComboBox<String> cbSignatures;
	private java.util.ArrayList<ASiCSignature> objects;
	private JPanel pSignatureInfo;
	private JTextField edLevel;
	private JTextField edSignatureType;
	private JTextField edIssuerRDN;
	private JTextField edSerialNumber;
	private JTextField edSubjectKeyID;
	private JTextField edTimestamp;
	private JTextField edSignedFiles;
	private JLabel lSignatureValidationResult;
	private JLabel lChainValidationResult;

	private Asicverifier verifier;

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
	public validationresultdialog() {
		setTitle("Validation results");
		setBounds(100, 100, 377, 550);
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
					ASiCSignature sig = objects.get(cbSignatures.getSelectedIndex());

					switch (sig.getLevel())
					{
						case ASiCSignature.aslBES: edLevel.setText("BES"); break;
						case ASiCSignature.aslEPES: edLevel.setText("EPES"); break;
						case ASiCSignature.aslT: edLevel.setText("T"); break;
						case ASiCSignature.aslC: edLevel.setText("C"); break;
						case ASiCSignature.aslXType1: edLevel.setText("XType1"); break;
						case ASiCSignature.aslXType2: edLevel.setText("XType2"); break;
						case ASiCSignature.aslXLType1: edLevel.setText("XLType1"); break;
						case ASiCSignature.aslXLType2: edLevel.setText("XLType2"); break;
						case ASiCSignature.aslBaselineB: edLevel.setText("Baseline B"); break;
						case ASiCSignature.aslBaselineT: edLevel.setText("Baseline T"); break;
						case ASiCSignature.aslBaselineLT: edLevel.setText("Baseline LT"); break;
						case ASiCSignature.aslBaselineLTA: edLevel.setText("Baseline LTA"); break;
						case ASiCSignature.aslExtendedBES: edLevel.setText("Extended BES"); break;
						case ASiCSignature.aslExtendedEPES: edLevel.setText("Extended EPES"); break;
						case ASiCSignature.aslExtendedT: edLevel.setText("Extended T"); break;
						case ASiCSignature.aslExtendedC: edLevel.setText("Extended C"); break;
						case ASiCSignature.aslExtendedXType1: edLevel.setText("Extended XType1"); break;
						case ASiCSignature.aslExtendedXType2: edLevel.setText("Extended XType2"); break;
						case ASiCSignature.aslExtendedXLType1: edLevel.setText("Extended XLType1"); break;
						case ASiCSignature.aslExtendedXLType2: edLevel.setText("Extended XLType2"); break;
						case ASiCSignature.aslA: edLevel.setText("A"); break;
						case ASiCSignature.aslExtendedA: edLevel.setText("Extended A"); break;

						default: edLevel.setText("Unknown"); break;
					}

					switch (sig.getSignatureType())
					{
						case ASiCSignature.castCAdES: edSignatureType.setText("CAdES"); break;
						case ASiCSignature.castXAdES: edSignatureType.setText("XAdES"); break;
						case ASiCSignature.castTimestamp: edSignatureType.setText("Timestamp"); break;
						default: edSignatureType.setText("Unknown"); break;
					}

					edIssuerRDN.setText(sig.getIssuerRDN());
					edSerialNumber.setText(bytesToHex(sig.getSerialNumber()));
					edSubjectKeyID.setText(bytesToHex(sig.getSubjectKeyID()));
					if (sig.getSignatureType() == ASiCSignature.castTimestamp)
						edTimestamp.setText(sig.getClaimedSigningTime());
					else
						edTimestamp.setText("");

					edSignedFiles.setText(sig.getSignedFiles());

					String s = "";
					switch (sig.getSignatureValidationResult())
					{
						case ASiCSignature.svtValid:  s = "Valid"; break;
						case ASiCSignature.svtCorrupted: s = "Corrupted"; break;
						case ASiCSignature.svtSignerNotFound: s = "SignerNotFound"; break;
						case ASiCSignature.svtFailure: s = "Failure"; break;
						default: s = "Unknown"; break;
					}

					lSignatureValidationResult.setText("Signature Validation Result: " + s);

					s = "";
					switch (sig.getChainValidationResult())
					{
						case ASiCSignature.cvtValid: s = "Valid"; break;
						case ASiCSignature.cvtValidButUntrusted: s = "ValidButUntrusted"; break;
						case ASiCSignature.cvtInvalid: s = "Invalid"; break;
						case ASiCSignature.cvtCantBeEstablished: s = "CantBeEstablished"; break;
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
		pSignatureInfo.setBounds(10, 65, 349, 430);
		contentPanel.add(pSignatureInfo);
		pSignatureInfo.setLayout(null);

		JLabel lLevel = new JLabel("Level");
		lLevel.setBounds(0, 0, 217, 14);
		pSignatureInfo.add(lLevel);

		edLevel = new JTextField();
		edLevel.setEditable(false);
		edLevel.setColumns(10);
		edLevel.setBounds(0, 20, 348, 20);
		pSignatureInfo.add(edLevel);

		JLabel lSignatureType = new JLabel("Signature type");
		lSignatureType.setBounds(0, 50, 217, 14);
		pSignatureInfo.add(lSignatureType);

		edSignatureType = new JTextField();
		edSignatureType.setEditable(false);
		edSignatureType.setColumns(10);
		edSignatureType.setBounds(0, 70, 348, 20);
		pSignatureInfo.add(edSignatureType);

		JLabel lIssuerRDN = new JLabel("IssuerRDN");
		lIssuerRDN.setBounds(0, 100, 217, 14);
		pSignatureInfo.add(lIssuerRDN);

		edIssuerRDN = new JTextField();
		edIssuerRDN.setEditable(false);
		edIssuerRDN.setColumns(10);
		edIssuerRDN.setBounds(0, 120, 348, 20);
		pSignatureInfo.add(edIssuerRDN);

		JLabel lSerialNumber = new JLabel("SerialNumber");
		lSerialNumber.setBounds(0, 150, 217, 14);
		pSignatureInfo.add(lSerialNumber);

		edSerialNumber = new JTextField();
		edSerialNumber.setEditable(false);
		edSerialNumber.setColumns(10);
		edSerialNumber.setBounds(0, 170, 348, 20);
		pSignatureInfo.add(edSerialNumber);

		JLabel lSubjectKeyID = new JLabel("SubjectKeyID");
		lSubjectKeyID.setBounds(0, 200, 217, 14);
		pSignatureInfo.add(lSubjectKeyID);

		edSubjectKeyID = new JTextField();
		edSubjectKeyID.setEditable(false);
		edSubjectKeyID.setColumns(10);
		edSubjectKeyID.setBounds(0, 220, 348, 20);
		pSignatureInfo.add(edSubjectKeyID);

		JLabel lTimestamp = new JLabel("Timestamp");
		lTimestamp.setBounds(0, 250, 217, 14);
		pSignatureInfo.add(lTimestamp);

		edTimestamp = new JTextField();
		edTimestamp.setEditable(false);
		edTimestamp.setColumns(10);
		edTimestamp.setBounds(0, 270, 348, 20);
		pSignatureInfo.add(edTimestamp);

		JLabel lSignedFiles = new JLabel("Signed files");
		lSignedFiles.setBounds(0, 300, 217, 14);
		pSignatureInfo.add(lSignedFiles);

		edSignedFiles = new JTextField();
		edSignedFiles.setEditable(false);
		edSignedFiles.setColumns(10);
		edSignedFiles.setBounds(0, 320, 348, 20);
		pSignatureInfo.add(edSignedFiles);

		lSignatureValidationResult = new JLabel("SignatureValidationResult");
		lSignatureValidationResult.setBounds(0, 350, 348, 14);
		pSignatureInfo.add(lSignatureValidationResult);

		lChainValidationResult = new JLabel("ChainValidationResult");
		lChainValidationResult.setBounds(0, 380, 348, 14);
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

		objects = new java.util.ArrayList<ASiCSignature>();
	}

	public void Init(Asicverifier _verifier) {
		verifier = _verifier;
		cbSignatures.removeAll();
		objects.clear();
		pSignatureInfo.setVisible(false);
		for (int i = 0; i < verifier.getSignatures().size(); i++)
		{
			objects.add(verifier.getSignatures().item(i));
			cbSignatures.addItem("Signature" + (i + 1));
		}
	}
}
