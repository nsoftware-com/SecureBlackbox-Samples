import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import secureblackbox.*;

class addresses extends JDialog {

    // Returns "null" if the user cancels the dialog;
    // otherwise returns updated addresses
    static String execute(JFrame parent, String title, String value) {
        addresses dialog = new addresses(parent, title);
        dialog.setAddresses(value);
        dialog.showModal();
        String newValue = dialog.result;
        dialog.dispose();
        return newValue;
    }

    private addresses(JFrame parent, String title) {
        super(parent, String.format("%s Addresses", title));
        setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
        setLayout(null);
        setResizable(false);
        setSize(450, 320);
        setLocationRelativeTo(parent);

        setVisible(true);
        setVisible(false);
        initializeControls();
        repaint();
    }

    private void showModal() {
        setModalityType(ModalityType.APPLICATION_MODAL);
        setVisible(true);
    }

    private void getAddresses() {
        String text = textAddress.getText();
        if (text != null && text.length() != 0)
            buttonAddClick();

        int count = modelList.getRowCount();
        if (count == 0) {
            result = "";
            return;
        }

        Mailwriter writer = new Mailwriter();
        MailAddressList list = writer.getFrom();
        for (int i = 0; i < count; i++) {
            String name = (String)modelList.getValueAt(i, 0);
            String address = (String)modelList.getValueAt(i, 1);
            list.add(new MailAddress(name, address));
        }

        result = writer.getMessage().getFrom();
    }

    private void setAddresses(String value) {
        Mailwriter writer = new Mailwriter();
        try {
            // all address fields in Mailwriter behave the same way, so there is
            // no difference which one to use to split a string into an address list
            writer.getMessage().setFrom(value);
        }
        catch (SecureBlackboxException err) {
            return;
        }

        MailAddressList list = writer.getFrom();
        modelList.setRowCount(list.size());
        for (int i = 0; i < list.size(); i++) {
            MailAddress addr = list.get(i);
            modelList.setValueAt(addr.getDisplayName(), i, 0);
            modelList.setValueAt(addr.getAddress(), i, 1);
        }
    }

    private void buttonAddClick() {
        String address = textAddress.getText();
        if (address == null || address.length() == 0) {
            JOptionPane.showMessageDialog(this, "E-mail Address is required in order to add an item.",
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        String name = textName.getText();
        int index = modelList.getRowCount();
        modelList.setRowCount(index + 1);
        modelList.setValueAt(name, index, 0);
        modelList.setValueAt(address, index, 1);

        textName.setText("");
        textAddress.setText("");
    }

    private void buttonDeleteClick() {
        int index = tableList.getSelectedRow();
        if (index == -1)
            return;

        modelList.removeRow(index);
        int count = modelList.getRowCount();
        if (count == 0)
            return;

        if (index < count)
            tableList.setRowSelectionInterval(index, index);
        else
            tableList.setRowSelectionInterval(index - 1, index - 1);
    }

    private void buttonOKClick() {
        getAddresses();
        setVisible(false);
    }

    private void buttonCancelClick() {
        result = null;
        setVisible(false);
    }

    private void initializeControls() {
        Container content = getContentPane();
        int h = content.getHeight();
        int w = content.getWidth();

        JLabel label = new JLabel("Name:");
        label.setSize(40, 18);
        label.setLocation(mailwriter.GAP, mailwriter.GAP);
        add(label);

        textName = new JTextField();
        textName.setLocation(label.getX(), mailwriter.getBottom(label) + mailwriter.HGAP);
        textName.setSize((w - mailwriter.TGAP) / 2, 25);
        add(textName);

        label = new JLabel("E-mail Addresses:");
        label.setSize(110, 18);
        label.setLocation(mailwriter.getRight(textName) + mailwriter.GAP, mailwriter.GAP);
        add(label);

        textAddress = new JTextField();
        textAddress.setLocation(label.getX(), textName.getY());
        textAddress.setSize(textName.getSize());
        add(textAddress);

        buttonAdd = new JButton("Add");
        buttonAdd.setSize(75, 25);
        buttonAdd.setLocation(mailwriter.getRight(textAddress) - buttonAdd.getWidth(),
                mailwriter.getBottom(textAddress) + mailwriter.GAP);
        buttonAdd.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonAddClick();
            }
        });
        add(buttonAdd);

        buttonDelete = new JButton("Delete");
        buttonDelete.setSize(75, 25);
        buttonDelete.setLocation(mailwriter.GAP, h - buttonDelete.getHeight() - mailwriter.GAP);
        buttonDelete.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonDeleteClick();
            }
        });

        scrollList = new JScrollPane();
        scrollList.setLocation(mailwriter.GAP, mailwriter.getBottom(buttonAdd) + mailwriter.DGAP);
        scrollList.setSize(w - mailwriter.DGAP, buttonDelete.getY() - scrollList.getY() - mailwriter.GAP);
        modelList = new DefaultTableModel(COLUMNS, 0);
        tableList = new JTable(modelList);
        scrollList.setViewportView(tableList);
        add(scrollList);

        label = new JLabel("Addresses:");
        label.setSize(70, 18);
        label.setLocation(scrollList.getX(), scrollList.getY() - label.getHeight() - mailwriter.HGAP);
        add(label);

        add(buttonDelete);

        buttonCancel = new JButton("Cancel");
        buttonCancel.setSize(75, 25);
        buttonCancel.setLocation(w - buttonCancel.getWidth() - mailwriter.GAP,
                h - buttonCancel.getHeight() - mailwriter.GAP);
        buttonCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonCancelClick();
            }
        });

        buttonOK = new JButton("OK");
        buttonOK.setSize(75, 25);
        buttonOK.setLocation(buttonCancel.getX() - mailwriter.GAP - buttonOK.getWidth(), buttonCancel.getY());
        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonOKClick();
            }
        });
        add(buttonOK);

        add(buttonCancel);
    }

    private String result;

    private JTextField textName;
    private JTextField textAddress;
    private JButton buttonAdd;
    private JScrollPane scrollList;
    private DefaultTableModel modelList;
    private JTable tableList;
    private JButton buttonDelete;
    private JButton buttonOK;
    private JButton buttonCancel;

    private static final String[] COLUMNS = { "Name", "Address" };
}
