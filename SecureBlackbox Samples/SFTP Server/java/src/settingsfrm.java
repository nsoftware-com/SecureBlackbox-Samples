import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.nio.file.Files;
import java.nio.file.Paths;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import secureblackbox.*;

public class settingsfrm extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable table;

	private Sftpserver serv;
	
	public settingsfrm(Sftpserver Serv) {
		setTitle("Server Settings");
		setBounds(100, 100, 566, 293);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JPanel panel = new JPanel();
			panel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED, null, null), "Autorized users", TitledBorder.LEADING, TitledBorder.TOP, null, null));
			panel.setBounds(10, 10, 538, 200);
			contentPanel.add(panel);
			panel.setLayout(null);
			{
				JButton btnAdd = new JButton("Add");
				btnAdd.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						addUserClick();
					}
				});
				btnAdd.setBounds(445, 22, 80, 25);
				panel.add(btnAdd);
			}
			{
				JButton btnRemove = new JButton("Remove");
				btnRemove.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						removeUserClick();
					}
				});
				btnRemove.setBounds(445, 52, 80, 25);
				panel.add(btnRemove);
			}
			{
				JScrollPane scrollPane = new JScrollPane();
				scrollPane.setBounds(10, 22, 424, 170);
				panel.add(scrollPane);
				{
					table = new JTable(new usertablemodel());
					table.setFillsViewportHeight(true);
					scrollPane.setViewportView(table);
				}
			}
		}
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				JButton okButton = new JButton("OK");
				okButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						setVisible(false);
					}
				});
				okButton.setActionCommand("OK");
				buttonPane.add(okButton);
				getRootPane().setDefaultButton(okButton);
			}
		}

		serv = Serv;
		FillUsers();
	}

	protected void addUserClick() {
		adduserdialog dlg = new adduserdialog();
		dlg.setModal(true);
		dlg.setVisible(true);
		if (dlg.IsOk)
			try
			{
				UserAccount user = new UserAccount();

				user.setUsername(dlg.edUserName.getText());
				user.setPassword(dlg.tbPass.getText());

				if (!dlg.edPublicKeyFile.getText().isEmpty())
				{
					byte[] sshkey = Files.readAllBytes(Paths.get(dlg.edPublicKeyFile.getText()));
					user.setSSHKey(sshkey);
				}

				serv.getUsers().add(user);
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		FillUsers();		
	}

	protected void removeUserClick() {
		usertablemodel model = (usertablemodel)table.getModel();
		serv.getUsers().remove(table.getSelectedRow());
		FillUsers();
	}

	
	// sets up users list view according to user accounts settings
	private void FillUsers()
	{
		clearUserTable();

		for (int i = 0; i < serv.getUsers().size(); i++)
		{
			usertablemodel model = (usertablemodel) table.getModel();
			model.addRow(serv.getUsers().item(i));
		}
	}

	private void clearUserTable() {
		usertablemodel model = (usertablemodel) table.getModel();
		model.clear();
	}
}