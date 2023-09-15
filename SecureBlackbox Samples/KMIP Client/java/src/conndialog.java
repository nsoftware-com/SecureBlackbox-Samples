import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;

import secureblackbox.Kmipclient;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class conndialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTextField tbHost;
	private JSpinner tbPort;
	private final ButtonGroup buttonGroup = new ButtonGroup();
	private JRadioButton rbTTLV;
	private JRadioButton rbXML;
	private JRadioButton rbJSON;
	private JTextField tbUser;
	private JPasswordField tbPassword;
	protected boolean isOk;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			conndialog dialog = new conndialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public conndialog() {
		setTitle("Connection properties");
		setBounds(100, 100, 395, 270);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JPanel panel = new JPanel();
			panel.setBorder(new TitledBorder(null, "Connection properties", TitledBorder.LEADING, TitledBorder.TOP, null, null));
			panel.setBounds(10, 11, 360, 170);
			contentPanel.add(panel);
			panel.setLayout(null);

			JLabel lblHost = new JLabel("Host");
			lblHost.setBounds(10, 21, 46, 14);
			panel.add(lblHost);

			tbHost = new JTextField();
			tbHost.setText("127.0.0.1");
			tbHost.setBounds(10, 40, 255, 20);
			panel.add(tbHost);
			tbHost.setColumns(10);

			JLabel lblPort = new JLabel("Port");
			lblPort.setBounds(275, 21, 46, 14);
			panel.add(lblPort);

			tbPort = new JSpinner();
			tbPort.setModel(new SpinnerNumberModel(new Integer(5696), null, null, new Integer(1)));
			tbPort.setBounds(275, 40, 74, 20);
			panel.add(tbPort);

			JPanel panel_4 = new JPanel();
			panel_4.setBorder(new TitledBorder(null, "Encoder type", TitledBorder.LEADING, TitledBorder.TOP, null, null));
			panel_4.setBounds(8, 70, 300, 45);
			panel.add(panel_4);
			panel_4.setLayout(null);

			rbTTLV = new JRadioButton("TTLV");
			buttonGroup.add(rbTTLV);
			rbTTLV.setSelected(true);
			rbTTLV.setHorizontalAlignment(SwingConstants.LEFT);
			rbTTLV.setBounds(10, 20, 60, 17);
			panel_4.add(rbTTLV);

			rbXML = new JRadioButton("XML");
			buttonGroup.add(rbXML);
			rbXML.setBounds(110, 20, 60, 17);
			panel_4.add(rbXML);

			rbJSON = new JRadioButton("JSON");
			buttonGroup.add(rbJSON);
			rbJSON.setBounds(210, 20, 60, 17);
			panel_4.add(rbJSON);

			JLabel lblUsername = new JLabel("Username");
			lblUsername.setBounds(10, 126, 84, 14);
			panel.add(lblUsername);

			tbUser = new JTextField();
			tbUser.setText("anonymous");
			tbUser.setBounds(8, 143, 158, 20);
			panel.add(tbUser);
			tbUser.setColumns(10);

			JLabel lblPassword = new JLabel("Password");
			lblPassword.setBounds(191, 126, 98, 14);
			panel.add(lblPassword);

			tbPassword = new JPasswordField();
			tbPassword.setBounds(191, 143, 158, 20);
			panel.add(tbPassword);
		}

		JButton btnOk = new JButton("Ok");
		btnOk.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				isOk = true;
				setVisible(false);
			}
		});
		btnOk.setBounds(200, 200, 80, 25);
		contentPanel.add(btnOk);

		JButton btnCancel = new JButton("Cancel");
		btnCancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				isOk = false;
				setVisible(false);
			}
		});
		btnCancel.setBounds(290, 200, 80, 25);
		contentPanel.add(btnCancel);
	}

	public boolean isOK() {
		return isOk;
	}

	public String getHost() {
		return tbHost.getText();
	}

	public int getPort() {
		return (Integer)tbPort.getValue();
	}

	public int getEncoderType()
	{
		if (rbJSON.isSelected())
		{
			return Kmipclient.etJSON;
		}
		else
		if (rbXML.isSelected())
		{
			return Kmipclient.etXML;
		}
		else
		{
			return Kmipclient.etTTLV;
		}
	}

	public String getUsername() {
		return tbUser.getText();
	}

	public String getPassword() {
		return new String(tbPassword.getPassword());
	}
}
