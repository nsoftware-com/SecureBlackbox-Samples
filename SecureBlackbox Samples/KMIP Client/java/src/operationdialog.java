import secureblackbox.Kmipclient;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import static secureblackbox.Kmipclient.*;

public class operationdialog extends JDialog {

    public static final int Op_Encrypt = 1;
    public static final int Op_Decrypt = 2;
    public static final int Op_Sign = 3;
    public static final int Op_Verify = 4;

    private static final long serialVersionUID = 1L;
    private final JPanel contentPanel = new JPanel();
    private JTextField tbInputFile;
    private JTextField tbOutputFile;
    private JComboBox cmbHashAlgorithm;

    private Kmipclient client;
    private String uniqueIdentifier;
    private int operation;

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
    public operationdialog(Kmipclient _client, String _uniqueIdentifier, int _operation)
    {
        client = _client;
        uniqueIdentifier = _uniqueIdentifier;
        operation = _operation;

        setBounds(100, 100, 500, 220);
        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        contentPanel.setLayout(null);

        JLabel lblInputFile = new JLabel("Input file:");
        lblInputFile.setBounds(10, 28, 80, 14);
        contentPanel.add(lblInputFile);

        tbInputFile = new JTextField("");
        tbInputFile.setBounds(95, 25, 300, 20);
        contentPanel.add(tbInputFile);
        tbInputFile.setColumns(10);

        JButton btnInputFile = new JButton("Browse");
        btnInputFile.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                tbInputFile.setText(getOpenFileName());
            }
        });
        btnInputFile.setBounds(400, 23, 80, 25);
        contentPanel.add(btnInputFile);

        JLabel lblOutputFile = new JLabel("Output file");
        lblOutputFile.setBounds(10, 58, 80, 14);
        contentPanel.add(lblOutputFile);

        tbOutputFile = new JTextField("");
        tbOutputFile.setBounds(95, 55, 300, 20);
        contentPanel.add(tbOutputFile);
        tbOutputFile.setColumns(10);

        JButton btnOutputFile = new JButton("Browse");
        btnOutputFile.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
                if (operation == Op_Verify)
                {
                    tbOutputFile.setText(getOpenFileName());
                }
                else
                {
                    tbOutputFile.setText(getSaveFileName());
                }
            }
        });
        btnOutputFile.setBounds(400, 53, 80, 25);
        contentPanel.add(btnOutputFile);

        JLabel lblHashAlgorithm = new JLabel("Hash algorithm:");
        lblHashAlgorithm.setBounds(10, 93, 100, 14);
        contentPanel.add(lblHashAlgorithm);

        cmbHashAlgorithm = new JComboBox<String>();
        cmbHashAlgorithm.setModel(new DefaultComboBoxModel<String>(new String[] {"SHA1", "MD5", "MD2", "SHA256", "SHA384", "SHA512"}));
        cmbHashAlgorithm.setBounds(110, 90, 140, 22);
        contentPanel.add(cmbHashAlgorithm);


        JButton btnOk = new JButton("Ok");
        btnOk.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                switch (operation) {
                    case Op_Encrypt:
                        if (tbInputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the input file");
                            return;
                        }

                        if (!(new File(tbInputFile.getText()).exists()))
                        {
                            showErrorMessage("Input file not found");
                            return;
                        }

                        if (tbOutputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the output file");
                            return;
                        }

                        try
                        {
                            client.setInputFile(tbInputFile.getText());
                            client.setOutputFile(tbOutputFile.getText());

                            client.encrypt(uniqueIdentifier);

                            showMessage("The file was encrypted successfully", "Info");

                        }
                        catch (Exception E)
                        {
                            showErrorMessage(E.getMessage());
                        }

                        break;
                    case Op_Decrypt:
                        if (tbInputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the input file");
                            return;
                        }

                        if (!(new File(tbInputFile.getText()).exists()))
                        {
                            showErrorMessage("Input file not found");
                            return;
                        }

                        if (tbOutputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the output file");
                            return;
                        }

                        try
                        {
                            client.setInputFile(tbInputFile.getText());
                            client.setOutputFile(tbOutputFile.getText());

                            client.decrypt(uniqueIdentifier);

                            showMessage("The file was decrypted successfully", "Info");
                        }
                        catch (Exception E)
                        {
                            showErrorMessage(E.getMessage());
                        }

                        break;
                    case Op_Sign:
                        if (tbInputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the input file");
                            return;
                        }

                        if (!(new File(tbInputFile.getText()).exists()))
                        {
                            showErrorMessage("Input file not found");
                            return;
                        }

                        if (tbOutputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the output file");
                            return;
                        }

                        try
                        {
                            client.setInputFile(tbInputFile.getText());
                            client.setOutputFile(tbOutputFile.getText());
                            client.config("HashAlgorithm=" + cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));

                            client.sign(uniqueIdentifier);

                            showMessage("The file was signed successfully", "Info");
                        }
                        catch (Exception E)
                        {
                            showErrorMessage(E.getMessage());
                        }

                        break;
                    case Op_Verify:
                        if (tbInputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the input file");
                            return;
                        }

                        if (!(new File(tbInputFile.getText()).exists()))
                        {
                            showErrorMessage("Input file not found");
                            return;
                        }

                        if (tbOutputFile.getText().isEmpty())
                        {
                            showErrorMessage("Please provide a valid name for the signature file");
                            return;
                        }

                        if (!(new File(tbOutputFile.getText()).exists()))
                        {
                            showErrorMessage("Signature file not found");
                            return;
                        }

                        try
                        {
                            client.setInputFile(tbInputFile.getText());
                            client.setDataFile(tbOutputFile.getText());
                            client.config("HashAlgorithm=" + cmbHashAlgorithm.getItemAt(cmbHashAlgorithm.getSelectedIndex()));

                            client.verify(uniqueIdentifier);

                            switch (client.getSignatureValidationResult())
                            {
                                case svtValid:
                                    showMessage("Verification succeeded", "Info");
                                    break;
                                case svtCorrupted:
                                    showErrorMessage("Verification corrupted");
                                    break;
                                case svtFailure:
                                    showErrorMessage("Verification failed");
                                    break;
                                default:
                                    showErrorMessage("Verification unknown");
                                    break;
                            }
                        }
                        catch (Exception E)
                        {
                            showErrorMessage(E.getMessage());
                        }


                        break;
                }

                setVisible(false);
            }
        });
        btnOk.setBounds(310, 150, 80, 25);
        contentPanel.add(btnOk);

        JButton btnCancel = new JButton("Cancel");
        btnCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                setVisible(false);
            }
        });
        btnCancel.setBounds(400, 150, 80, 25);
        contentPanel.add(btnCancel);


        switch (operation)
        {
            case Op_Encrypt:
                setTitle("Encrypt");
                btnOk.setText("Encrypt");
                lblOutputFile.setText("Output file:");
                lblHashAlgorithm.setEnabled(false);
                cmbHashAlgorithm.setEnabled(false);
                break;
            case Op_Decrypt:
                setTitle("Decrypt");
                btnOk.setText("Decrypt");
                lblOutputFile.setText("Output file:");
                lblHashAlgorithm.setEnabled(false);
                cmbHashAlgorithm.setEnabled(false);
                break;
            case Op_Sign:
                setTitle("Sign");
                btnOk.setText("Sign");
                lblOutputFile.setText("Output file:");
                lblHashAlgorithm.setEnabled(true);
                cmbHashAlgorithm.setEnabled(true);
                break;
            case Op_Verify:
                setTitle("Verify");
                btnOk.setText("Verify");
                lblOutputFile.setText("Signature file:");
                lblHashAlgorithm.setEnabled(true);
                cmbHashAlgorithm.setEnabled(true);
                break;
        }
    }

    String getSaveFileName(){
        JFileChooser fc = new JFileChooser();
        int returnVal = fc.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION)
            return fc.getSelectedFile().getPath();

        return "";
    }

    String getOpenFileName(){
        JFileChooser fc = new JFileChooser();
        int returnVal = fc.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION)
            return fc.getSelectedFile().getPath();

        return "";
    }

    static void showMessage(String msg, String caption) {
        JOptionPane.showMessageDialog(null, msg, caption, JOptionPane.INFORMATION_MESSAGE);
    }

    static void showErrorMessage(String msg) {
        JOptionPane.showMessageDialog(null, msg, "Error", JOptionPane.ERROR_MESSAGE);
    }
}
