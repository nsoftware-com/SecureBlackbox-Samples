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
import java.util.ArrayList;

public class refsdialog extends JDialog {

	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JTable table;

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
	public refsdialog(ArrayList referenceResult) {
		setTitle("References");
		setBounds(100, 100, 330, 300);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JScrollPane scrollPane = new JScrollPane();
			scrollPane.setBounds(5, 10, 305, 210);
			contentPanel.add(scrollPane);
			{
				table = new JTable();
				table.setModel(new  DefaultTableModel(
						new Object[][] {
						},
						new String[] {
								"Id", "Uri", "RefType", "DigestValid"
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
		for (int i = 0; i < referenceResult.size(); i++)
			addRow(((referenceres)referenceResult.get(i)).Id, ((referenceres)referenceResult.get(i)).Uri,
					((referenceres)referenceResult.get(i)).RefType, ((referenceres)referenceResult.get(i)).DigestValid);
	}

	private void addRow(String id, String uri, String refType, Boolean digestValid) {
		DefaultTableModel model = (DefaultTableModel)table.getModel();
		model.insertRow(model.getRowCount(), new Object[] {id, uri, refType, digestValid});
	}

	private void clearTable() {
		((DefaultTableModel)table.getModel()).setNumRows(0);
	}
}
