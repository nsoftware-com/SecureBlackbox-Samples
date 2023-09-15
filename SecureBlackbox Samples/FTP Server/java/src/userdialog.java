import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JPasswordField;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import secureblackbox.*;


public class userdialog extends JDialog {
	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTextField edUsername;
	private JPasswordField edPassword;
	private JTextField edHomeDirectory;
	private JTextField edSpeedLimit;
	private boolean isOK;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			userdialog dialog = new userdialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public userdialog() {
		setTitle("User settings");
		setBounds(100, 100, 450, 229);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		
		JLabel lblUsername = new JLabel("Username");
		lblUsername.setBounds(10, 11, 136, 14);
		contentPanel.add(lblUsername);
		
		JLabel lblPassword = new JLabel("Password");
		lblPassword.setBounds(226, 11, 91, 14);
		contentPanel.add(lblPassword);
		
		edUsername = new JTextField();
		edUsername.setBounds(10, 29, 176, 20);
		contentPanel.add(edUsername);
		edUsername.setColumns(10);
		
		edPassword = new JPasswordField();
		edPassword.setBounds(226, 29, 176, 20);
		contentPanel.add(edPassword);
		
		JLabel lblHomeDirectory = new JLabel("Home directory");
		lblHomeDirectory.setBounds(10, 60, 165, 14);
		contentPanel.add(lblHomeDirectory);
		
		edHomeDirectory = new JTextField();
		edHomeDirectory.setBounds(10, 85, 422, 20);
		contentPanel.add(edHomeDirectory);
		edHomeDirectory.setColumns(10);
		
		JLabel lblSpeedLimit = new JLabel("Speed limit");
		lblSpeedLimit.setBounds(10, 116, 176, 14);
		contentPanel.add(lblSpeedLimit);
		
		edSpeedLimit = new JTextField();
		edSpeedLimit.setBounds(10, 135, 86, 20);
        edSpeedLimit.setText("0");
		contentPanel.add(edSpeedLimit);
		edSpeedLimit.setColumns(10);
		
		JLabel lblKbsec = new JLabel("Kb/sec");
		lblKbsec.setBounds(106, 141, 46, 14);
		contentPanel.add(lblKbsec);
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				JButton okButton = new JButton("OK");
				okButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						isOK = true;
						setVisible(false);
					}
				});
				okButton.setActionCommand("OK");
				buttonPane.add(okButton);
				getRootPane().setDefaultButton(okButton);
			}
			{
				JButton cancelButton = new JButton("Cancel");
				cancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						dispose();
					}
				});
				cancelButton.setActionCommand("Cancel");
				buttonPane.add(cancelButton);
			}
		}
	}

	private void initDialog(UserAccount user)
	{
        edHomeDirectory.setText(user.getBasePath());
        edUsername.setText(user.getUsername());
        edPassword.setText("********");
        edSpeedLimit.setText(user.getIncomingSpeedLimit() + "");
	}
	
	public static boolean editUserParameters(UserAccountList users, String userName)
	{
		for (int i = 0; i < users.size(); i++) {
			if (users.item(i).getUsername().equalsIgnoreCase(userName))
			{
				userdialog frm = new userdialog();
				frm.initDialog(users.item(i));
				frm.setModal(true);
				frm.setVisible(true);
				if (frm.isOk())
				{
					try
					{
						users.item(i).setBasePath(frm.getHomeDirectory());
						users.item(i).setIncomingSpeedLimit(Integer.parseInt(frm.getSpeedLimit()));
						users.item(i).setOutgoingSpeedLimit(Integer.parseInt(frm.getSpeedLimit()));
						if (frm.getPassword().compareTo("********") != 0)
							users.item(i).setPassword(frm.getPassword());
					}
					catch (Exception ex)
					{
						ex.printStackTrace();
					}

					frm.dispose();
					return true;
				}
				else
					{
					frm.dispose();
					return false;
				}
			}
		}
		return false;
	}
	
	public static boolean AddUserParameters(Ftpserver server)
	{
		userdialog frm = new userdialog();
		frm.setModal(true);
		frm.setVisible(true);
		if (frm.isOk())
		{
			try
			{
				UserAccount user = new UserAccount();

				user.setUsername(frm.getUsername());
				user.setPassword(frm.getPassword());
				user.setIncomingSpeedLimit(Integer.parseInt(frm.getSpeedLimit()));
				user.setOutgoingSpeedLimit(Integer.parseInt(frm.getSpeedLimit()));
				user.setBasePath(frm.getHomeDirectory());

				server.getUsers().add(user);
			}
			catch (Exception ex)
			{
				ex.printStackTrace();
			}
			frm.dispose();
			return true;
		}
		else {
			frm.dispose(); 
			return false;
		}
	}

	private String getPassword() {
		return new String(edPassword.getPassword());
	}

	private String getSpeedLimit() {
		return edSpeedLimit.getText();
	}

	private String getUsername() {
		return edUsername.getText();
	}

	private String getHomeDirectory() {
		return edHomeDirectory.getText();
	}

	private boolean isOk() {
		return isOK;
	}
}
