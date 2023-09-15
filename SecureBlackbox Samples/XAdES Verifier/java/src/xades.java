import java.awt.*;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;

import static secureblackbox.Xadesverifier.*;

class xades extends JDialog {
	private static final long serialVersionUID = -8938687539889635131L;
	private final JPanel contentPanel = new JPanel();

	private JComboBox<String> cmbVersion;
	private JComboBox<String> cmbForm;
	private JLabel lblCountry;
	private JTextField edCountry;
	private JLabel lblPostalCode;
	private JTextField edPostalCode;
	private JLabel lblStateOrProvince;
	private JTextField edStateOrProvince;
	private JLabel lblCity;
	private JTextField edCity;
	private JLabel lbSignedTime;
	private JLabel lbTimestamp;
	private JLabel lbTimestampSerial;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			xades dialog = new xades(null, 0, "", "");
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public xades(Xadesverifier verifier, Integer sigIndex, String Timestamp,  String TimestampSerial)
	{
		setTitle("XAdES Options");
		
		setBounds(100, 100, 320, 270);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);

		JLabel lblVersion = new JLabel("Version");
		lblVersion.setBounds(10, 23, 53, 14);
		contentPanel.add(lblVersion);

		cmbVersion = new JComboBox<String>();
		cmbVersion.setModel(new DefaultComboBoxModel<String>(new String[] {"1.1.1", "1.2.2", "1.3.2"}));
		cmbVersion.setBounds(75, 20, 94, 22);
		cmbVersion.setEnabled(false);
		contentPanel.add(cmbVersion);

		JLabel lblForm = new JLabel("Form");
		lblForm.setBounds(10, 53, 53, 14);
		contentPanel.add(lblForm);

		cmbForm = new JComboBox<String>();
		cmbForm.setModel(new DefaultComboBoxModel<String>(new String[] {"XAdES", "XAdES_BES", "XAdES_EPES", "XAdES_T", "XAdES_C", "XAdES_X", "XAdES_X_L", "XAdES_A", "XAdES_E_BES", "XAdES_E_EPES", "XAdES_E_T", "XAdES_E_C", "XAdES_E_X", "XAdES_E_X_Long", "XAdES_E_X_L", "XAdES_E_A"}));
		cmbForm.setBounds(75, 50, 150, 22);
		cmbForm.setEnabled(false);
		contentPanel.add(cmbForm);

		lbSignedTime = new JLabel("Signed Time:");
		lbSignedTime.setBounds(6, 100, 250, 23);
		contentPanel.add(lbSignedTime);

		lbTimestamp = new JLabel("Timestamp:");
		lbTimestamp.setBounds(6, 130, 250, 23);
		contentPanel.add(lbTimestamp);

		lbTimestampSerial = new JLabel("Timestamp Serial:");
		lbTimestampSerial.setBounds(6, 160, 250, 23);
		contentPanel.add(lbTimestampSerial);

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


		if (verifier != null) {
			XAdESSignature sig = verifier.getSignatures().get(sigIndex);
			switch (sig.getXAdESVersion()) {
				case XAdESSignature.xav111: {
					cmbVersion.setSelectedIndex(0);
					break;
				}
				case XAdESSignature.xav122: {
					cmbVersion.setSelectedIndex(1);
					break;
				}
				case XAdESSignature.xav132: {
					cmbVersion.setSelectedIndex(2);
					break;
				}
				default: {
					cmbVersion.setSelectedIndex(-1);
					break;
				}
			}

			switch (sig.getXAdESForm())
			{
				case XAdESSignature.xafBasic:
				{
					cmbForm.setSelectedIndex(0);
					break;
				}
				case XAdESSignature.xafBES:
				{
					cmbForm.setSelectedIndex(1);
					break;
				}
				case XAdESSignature.xafEPES:
				{
					cmbForm.setSelectedIndex(2);
					break;
				}
				case XAdESSignature.xafT:
				{
					cmbForm.setSelectedIndex(3);
					break;
				}
				case XAdESSignature.xafC:
				{
					cmbForm.setSelectedIndex(4);
					break;
				}
				case XAdESSignature.xafX:
				{
					cmbForm.setSelectedIndex(5);
					break;
				}
				case XAdESSignature.xafXL:
				{
					cmbForm.setSelectedIndex(6);
					break;
				}
				case XAdESSignature.xafA:
				{
					cmbForm.setSelectedIndex(7);
					break;
				}
				case XAdESSignature.xafExtendedBES:
				{
					cmbForm.setSelectedIndex(8);
					break;
				}
				case XAdESSignature.xafExtendedEPES:
				{
					cmbForm.setSelectedIndex(9);
					break;
				}
				case XAdESSignature.xafExtendedT:
				{
					cmbForm.setSelectedIndex(10);
					break;
				}
				case XAdESSignature.xafExtendedC:
				{
					cmbForm.setSelectedIndex(11);
					break;
				}
				case XAdESSignature.xafExtendedX:
				{
					cmbForm.setSelectedIndex(12);
					break;
				}
				case XAdESSignature.xafExtendedXLong:
				{
					cmbForm.setSelectedIndex(13);
					break;
				}
				case XAdESSignature.xafExtendedXL:
				{
					cmbForm.setSelectedIndex(14);
					break;
				}
				case XAdESSignature.xafExtendedA:
				{
					cmbForm.setSelectedIndex(15);
					break;
				}
				default:
				{
					cmbForm.setSelectedIndex(0);
					break;
				}
			}

			if (sig.getValidatedSigningTime() != "")
				lbSignedTime.setText("Validated Signing Time: " + sig.getValidatedSigningTime() + " UTC");
			else
			if (sig.getClaimedSigningTime() != "")
				lbSignedTime.setText("Claimed Signing Time: " + sig.getClaimedSigningTime() + " UTC");
			else
				lbSignedTime.setText("");

			lbTimestamp.setText(Timestamp);
			lbTimestampSerial.setText(TimestampSerial);
		}
	}
}