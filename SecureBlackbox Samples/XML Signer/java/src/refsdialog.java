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

public class refsdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable table;
	private XMLReferenceList FReferences = null;
	private JButton btnAdd;
	private JButton btnDelete;
	private JButton btnInfo;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			refsdialog dialog = new refsdialog(null);
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public refsdialog(XMLReferenceList references) {
		FReferences = references;

		setTitle("References");
		setBounds(100, 100, 310, 300);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JScrollPane scrollPane = new JScrollPane();
			scrollPane.setBounds(10, 10, 195, 210);
			contentPanel.add(scrollPane);
			{
				table = new JTable();
				table.setModel(new  DefaultTableModel(
						new Object[][] {
						},
						new String[] {
								"Id", "Uri"
						}
				));
				table.setFillsViewportHeight(true);
				scrollPane.setViewportView(table);
			}
		}
		{
			btnAdd = new JButton("Add");
			btnAdd.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					add();
				}
			});
			btnAdd.setBounds(210, 10, 80, 25);
			contentPanel.add(btnAdd);
		}
		{
			btnInfo = new JButton("Edit");
			btnInfo.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					edit();
				}
			});
			btnInfo.setBounds(210, 40, 80, 25);
			contentPanel.add(btnInfo);
		}
		{
			btnDelete = new JButton("Delete");
			btnDelete.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					delete();
				}
			});
			btnDelete.setBounds(210, 90, 80, 25);
			contentPanel.add(btnDelete);
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

		updateReferences();
	}

	protected void add()
	{
		XMLReference Ref = new XMLReference();

		refdialog frmReference= new refdialog();
		frmReference.setLocationRelativeTo(this);

		frmReference.Initialize(Ref);
		frmReference.setModal(true);
		frmReference.setVisible(true);

		if (frmReference.isOK())
		{
			frmReference.Update(Ref);
			FReferences.add(Ref);

			updateReferences();
		}

		frmReference.dispose();
	}

	protected void edit()
	{
		if (table.getSelectedRows().length > 0)
		{
			refdialog frmReference= new refdialog();
			frmReference.setLocationRelativeTo(this);

			frmReference.Initialize(FReferences.item(table.getSelectedRow()));
			frmReference.setModal(true);
			frmReference.setVisible(true);

			if (frmReference.isOK())
			{
				frmReference.Update(FReferences.item(table.getSelectedRow()));

				updateReferences();
			}

			frmReference.dispose();
		}
	}

	protected void delete()
	{
		int numRows = table.getSelectedRows().length;
		DefaultTableModel model = ((DefaultTableModel) table.getModel());
		for (int i = 0; i < numRows; i++ )
		{
			FReferences.remove(table.getSelectedRow());

			model.removeRow(table.getSelectedRow());
		}
	}

	private void updateReferences() {
		clearTable();
		for (int i = 0; i < FReferences.size(); i++)
			addRow(FReferences.item(i).getID(), FReferences.item(i).getURI());
	}

	private void addRow(String id, String uri) {
		DefaultTableModel model = (DefaultTableModel)table.getModel();
		model.insertRow(model.getRowCount(), new Object[] {id, uri});
	}

	private void clearTable() {
		((DefaultTableModel)table.getModel()).setNumRows(0);
	}
}
