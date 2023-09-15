/*
 * SecureBlackbox 2022 Java Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

import java.io.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.net.URLConnection;
import java.text.DecimalFormat;
import java.util.TooManyListenersException;
import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;

import secureblackbox.*;

public class mailwriter extends JFrame {

    private Mailwriter writer;

    public static void main(String[] args) {
        new mailwriter();
    }

    private mailwriter() {
        super("Mail Writer");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(null);
        setLocationByPlatform(true);
        setResizable(false);
        setSize(990, 700);
        setVisible(true);

        initializeControls();
        repaint();
        // a workaround to repaint comboboxes
        comboPriority.setVisible(false);
        comboPriority.setVisible(true);

        writer = new Mailwriter();
    }

    private void buttonFromClick() {
        String value = addresses.execute(this, "FROM", textFrom.getText());
        if (value != null)
            textFrom.setText(value);
    }

    private void buttonToClick() {
        String value = addresses.execute(this, "TO", textTo.getText());
        if (value != null)
            textTo.setText(value);
    }

    private void buttonCcClick() {
        String value = addresses.execute(this, "CC", textCc.getText());
        if (value != null)
            textCc.setText(value);
    }

    private void buttonBccClick() {
        String value = addresses.execute(this, "BCC", textBcc.getText());
        if (value != null)
            textBcc.setText(value);
    }

    private void buttonLoadSigningCertificateClick() {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Load a Signing Certificate");
        chooser.setMultiSelectionEnabled(false);
        FileNameExtensionFilter filter = new FileNameExtensionFilter("Certificates (*.pfx, *.p12)", "pfx", "p12");
        chooser.setFileFilter(filter);

        if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION)
            return;

        Certificatemanager manager = new Certificatemanager();
        try {
            manager.addCertificatemanagerEventListener(new DefaultCertificatemanagerEventListener() {
                    public void passwordNeeded(CertificatemanagerPasswordNeededEvent e) {
                        JPasswordField field = new JPasswordField();
                        if (JOptionPane.showConfirmDialog(null, field, "Enter a password:",
                                JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE) != JOptionPane.OK_OPTION) {
                            e.cancel = true;
                            return;
                        }
                        e.password = new String(field.getPassword());
                    }
                });
            manager.importFromFile(chooser.getSelectedFile().getPath(), "");
        }
        catch (TooManyListenersException err) {
            // just ignore this in the sample for simplicity
        }
        catch (SecureBlackboxException err) {
            if (err.getCode() == Constants.SB_ERROR_CANCELLED_BY_USER)
                return;

            JOptionPane.showMessageDialog(this, err.getMessage(), this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        Certificate certificate = manager.getCertificate();
        if (!certificate.getPrivateKeyExists()) {
            JOptionPane.showMessageDialog(this, "The certificate does not contain a private key, and cannot be used for signing.",
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        try {
            writer.setSigningCertificate(certificate);
        }
        catch (SecureBlackboxException err) {
            //
        }
        textSigningCertificate.setText(String.format("%s (%s %d bits)",
                certificate.getSubject(), certificate.getKeyAlgorithm(), certificate.getKeyBits()));
    }

    private void buttonClearSigningCertificateClick() {
        try {
            writer.setSigningCertificate(null);
        }
        catch (SecureBlackboxException err) {
            // just ignore this here
        }
        textSigningCertificate.setText("");
    }

    private void buttonLoadEncryptionCertificateClick() {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Load an Encryption Certificate");
        chooser.setMultiSelectionEnabled(false);
        FileNameExtensionFilter filter = new FileNameExtensionFilter("Certificates (*.cer, *.pem, *.pfx, *.p12)", "cer", "pem", "pfx", "p12");
        chooser.setFileFilter(filter);

        if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION)
            return;

        Certificatemanager manager = new Certificatemanager();
        try {
            manager.addCertificatemanagerEventListener(new DefaultCertificatemanagerEventListener() {
                public void passwordNeeded(CertificatemanagerPasswordNeededEvent e) {
                    JPasswordField field = new JPasswordField();
                    if (JOptionPane.showConfirmDialog(null, field, "Enter a password:",
                            JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE) != JOptionPane.OK_OPTION) {
                        e.cancel = true;
                        return;
                    }
                    e.password = new String(field.getPassword());
                }
            });
            manager.importFromFile(chooser.getSelectedFile().getPath(), "");
        }
        catch (TooManyListenersException err) {
            // just ignore this in the sample for simplicity
        }
        catch (SecureBlackboxException err) {
            if (err.getCode() == Constants.SB_ERROR_CANCELLED_BY_USER)
                return;

            JOptionPane.showMessageDialog(this, err.getMessage(), this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        Certificate certificate = manager.getCertificate();
        writer.getEncryptionCertificates().add(certificate);
        int index = modelEncryptionCertificates.getRowCount();
        modelEncryptionCertificates.setRowCount(index + 1);
        modelEncryptionCertificates.setValueAt(certificate.getSubject(), index, 0);
        modelEncryptionCertificates.setValueAt(certificate.getIssuer(), index, 1);
        tableEncryptionCertificates.setRowSelectionInterval(index, index);
    }

    private void buttonDeleteEncryptionCertificateClick() {
        int index = tableEncryptionCertificates.getSelectedRow();
        if (index == -1)
            return;

        writer.getEncryptionCertificates().remove(index);
        modelEncryptionCertificates.removeRow(index);

        int count = modelEncryptionCertificates.getRowCount();
        if (count == 0)
            return;

        if (index < count)
            tableEncryptionCertificates.setRowSelectionInterval(index, index);
        else
            tableEncryptionCertificates.setRowSelectionInterval(index - 1, index - 1);
    }

    private void buttonEmbedClick() {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Embed an Image");
        chooser.setMultiSelectionEnabled(false);
        FileNameExtensionFilter filter = new FileNameExtensionFilter("Images (*.gif, *.jpg, *.png)", "jpg", "gif", "png");
        chooser.setFileFilter(filter);

        if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION)
            return;

        String filename = chooser.getSelectedFile().getPath();
        byte[] buffer;
        try {
            buffer = loadFile(filename);
        }
        catch (IOException err) {
            JOptionPane.showMessageDialog(this, "Failed to load the file.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        String id = JOptionPane.showInputDialog(this, "Enter an ID for the image:");
        if (id == null)
            return;

        int index;
        MailAttachment attachment;
        try {
            index = writer.attachImage(id, buffer);
            attachment = writer.getAttachments().get(index);
            String ext = getExtension(filename);
            if (ext.equalsIgnoreCase("jpg"))
                attachment.setContentSubtype("jpeg");
            else
                attachment.setContentSubtype(ext);
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(this, "Failed to embed the image.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        modelAttachments.setRowCount(index + 1);
        modelAttachments.setValueAt(attachment.getID(), index, 0);
        String contentType = attachment.getContentType();
        if (contentType == null || contentType.length() == 0)
            modelAttachments.setValueAt("", index, 1);
        else
            modelAttachments.setValueAt(contentType + "/" + attachment.getContentSubtype(), index, 1);
        modelAttachments.setValueAt(attachment.getSize(), index, 2);
        tableAttachments.setRowSelectionInterval(index, index);
    }

    private void buttonAttachClick() {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Attach a File");
        chooser.setMultiSelectionEnabled(false);

        if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION)
            return;

        String filename = chooser.getSelectedFile().getPath();
        int index;
        try {
            index = writer.attachFile(filename);
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(this, "Failed to attach the file.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        MailAttachment attachment = writer.getAttachments().get(index);
        applyContentType(attachment);

        modelAttachments.setRowCount(index + 1);
        modelAttachments.setValueAt(attachment.getFilename(), index, 0);
        String contentType = attachment.getContentType();
        if (contentType == null || contentType.length() == 0)
            modelAttachments.setValueAt("", index, 1);
        else
            modelAttachments.setValueAt(contentType + "/" + attachment.getContentSubtype(), index, 1);
        modelAttachments.setValueAt(attachment.getSize(), index, 2);
        tableAttachments.setRowSelectionInterval(index, index);
    }

    private void buttonDeleteAttachmentClick() {
        int index = tableAttachments.getSelectedRow();
        if (index == -1)
            return;

        writer.getAttachments().remove(index);
        modelAttachments.removeRow(index);

        int count = modelAttachments.getRowCount();
        if (count == 0)
            return;

        if (index < count)
            tableAttachments.setRowSelectionInterval(index, index);
        else
            tableAttachments.setRowSelectionInterval(index - 1, index - 1);
    }

    private void buttonFilenameClick() {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Save a Message");
        chooser.setMultiSelectionEnabled(false);

        FileNameExtensionFilter filter = new FileNameExtensionFilter("E-mail Messages (*.eml, *.msg)", "eml", "msg");
        chooser.setFileFilter(filter);

        if (chooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION)
            textFilename.setText(chooser.getSelectedFile().getPath());
    }

    private void buttonSaveClick() {
        String filename = textFilename.getText();

        if (filename == null || filename.length() == 0) {
            JOptionPane.showMessageDialog(this, "Please specify a name for the destination file.",
                    this.getTitle(), JOptionPane.WARNING_MESSAGE);
            return;
        }

        File file = new File(filename);
        File directory = file.getParentFile();
        if (directory != null && !directory.exists()) {
            if (JOptionPane.showConfirmDialog(this, "The destination directory does not exist. Would you like to create it?",
                    this.getTitle(), JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION)
                return;

            if (!directory.mkdirs()) {
                JOptionPane.showMessageDialog(this, "Failed to create the destination directory.",
                        this.getTitle(), JOptionPane.ERROR_MESSAGE);
                return;
            }
        }

        MailMessage message = writer.getMessage();
        try {
            // originators
            message.setFrom(textFrom.getText());
            message.setSender(textSender.getText());
            // addressees
            message.setSendTo(textTo.getText());
            message.setCc(textCc.getText());
            message.setBcc(textBcc.getText());
            // options
            message.setSubject(textSubject.getText());
            switch (comboPriority.getSelectedIndex()) {
                case 0:
                    message.setPriority(MailMessage.mpLowest);
                    break;
                case 1:
                    message.setPriority(MailMessage.mpLow);
                    break;
                case 3:
                    message.setPriority(MailMessage.mpHigh);
                    break;
                case 4:
                    message.setPriority(MailMessage.mpHighest);
                    break;
                default:
                    message.setPriority(MailMessage.mpNormal);
            }
            message.setDeliveryReceipt(checkDeliveryReceipt.isSelected());
            message.setReadReceipt(checkReadReceipt.isSelected());
            // text
            message.setPlainText(textPlain.getText());
            message.setHtmlText(textHtml.getText());
            // security
            MailSecuritySettings settings = writer.getSecuritySettings();
            settings.setSignBeforeEncrypt(false);
            settings.setSignMessageHeader(false);
            settings.setSign(writer.getSigningCertificate() != null);
            if (settings.getSign()) {
                int index = comboHashAlgorithm.getSelectedIndex();
                if (index == -1) {
                    JOptionPane.showMessageDialog(this, "Please select a hash algorithm to sign the message.",
                            this.getTitle(), JOptionPane.WARNING_MESSAGE);
                    return;
                }
                settings.setHashAlgorithm(HASH_ALGORITHM_CONSTS[index]);
            }
            settings.setEncrypt(writer.getEncryptionCertificates().size() != 0);
            if (settings.getEncrypt()) {
                int index = comboEncryptionAlgorithm.getSelectedIndex();
                if (index == -1) {
                    JOptionPane.showMessageDialog(this, "Please select an encryption algorithm to encrypt the message.",
                            this.getTitle(), JOptionPane.WARNING_MESSAGE);
                    return;
                }
                settings.setEncryptionAlgorithm(ENCRYPTION_ALGORITHM_CONSTS[index]);
            }
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(this, "Failed to setup MailWriter.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        try {
            writer.saveToFile(filename);
        }
        catch (SecureBlackboxException err) {
            JOptionPane.showMessageDialog(this, "Failed to assemble and/or save a message.\n" + err.getMessage(),
                    this.getTitle(), JOptionPane.ERROR_MESSAGE);
            return;
        }

        JOptionPane.showMessageDialog(this, "A message has been assembled and saved successfully.",
                this.getTitle(), JOptionPane.INFORMATION_MESSAGE);
        try {
            // prepare a new message
            writer.setMessage(new MailMessage());
        }
        catch (SecureBlackboxException err) {
            //
        }
    }

    private void initializeControls() {
        Insets areaFrame = getClientArea(this);

        // Sample description label

        JLabel label = new JLabel("This sample shows how to compose an e-mail message. Also, it's possible to sign and/or encrypt the message with X.509 certificates.");
        label.setLocation(areaFrame.left + GAP, GAP);
        label.setSize(areaFrame.right - areaFrame.left - DGAP, 18);
        label.setForeground(SystemColor.textHighlight);
        this.add(label);

        // Originators groupbox

        panelOriginators = new JPanel(null);
        panelOriginators.setBorder(BorderFactory.createTitledBorder(" Originator(s) "));
        panelOriginators.setLocation(GAP, getBottom(label) + GAP);
        panelOriginators.setSize(430, 110);
        add(panelOriginators);
        Insets areaPanel = getClientArea(panelOriginators);

        label = new JLabel("From:");
        label.setLocation(areaPanel.left + GAP + 12, areaPanel.top + GAP + 2);
        label.setSize(32, 18);
        panelOriginators.add(label);

        textFrom = new JTextField();
        textFrom.setLocation(getRight(label) + GAP, areaPanel.top + GAP);
        textFrom.setEditable(false);
        panelOriginators.add(textFrom);

        buttonFrom = new JButton("...");
        buttonFrom.setMargin(new Insets(0, 0, 0, 0));
        buttonFrom.setSize(25, 25);
        buttonFrom.setLocation(areaPanel.right - GAP - buttonFrom.getWidth(), textFrom.getY());
        buttonFrom.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonFromClick();
            }
        });
        panelOriginators.add(buttonFrom);

        textFrom.setSize(buttonFrom.getX() - getRight(label) - DGAP, 25);

        label = new JLabel("Sender:");
        label.setLocation(areaPanel.left + GAP, getBottom(textFrom) + GAP + 2);
        label.setSize(44, 18);
        panelOriginators.add(label);

        textSender = new JTextField();
        textSender.setLocation(textFrom.getX(), getBottom(textFrom) + GAP);
        textSender.setSize(areaPanel.right - getRight(label) - DGAP, 25);
        panelOriginators.add(textSender);

        label = new JLabel("(required only if there are several addresses in the \"From\" field)");
        label.setLocation(textSender.getX(), getBottom(textSender));
        label.setSize(textSender.getWidth(), 18);
        label.setForeground(SystemColor.textInactiveText);
        Font font = label.getFont();
        label.setFont(font.deriveFont(Font.PLAIN));
        label.setHorizontalAlignment(SwingConstants.CENTER);
        panelOriginators.add(label);

        // Addressees groupbox

        panelAddressees = new JPanel(null);
        panelAddressees.setBorder(BorderFactory.createTitledBorder(" Addressees "));
        panelAddressees.setLocation(GAP, getBottom(panelOriginators) + GAP);
        panelAddressees.setSize(panelOriginators.getWidth(), 126);
        add(panelAddressees);
        areaPanel = getClientArea(panelAddressees);

        label = new JLabel("To:");
        label.setLocation(areaPanel.left + GAP + 8, areaPanel.top + GAP + 2);
        label.setSize(17, 18);
        panelAddressees.add(label);

        textTo = new JTextField();
        textTo.setLocation(getRight(label) + GAP, areaPanel.top + GAP);
        textTo.setEditable(false);
        panelAddressees.add(textTo);

        buttonTo = new JButton("...");
        buttonTo.setMargin(new Insets(0, 0, 0, 0));
        buttonTo.setSize(25, 25);
        buttonTo.setLocation(buttonFrom.getLocation());
        buttonTo.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonToClick();
            }
        });
        panelAddressees.add(buttonTo);

        textTo.setSize(buttonTo.getX() - getRight(label) - DGAP, 25);

        label = new JLabel("Cc:");
        label.setLocation(areaPanel.left + GAP + 7, getBottom(textTo) + GAP + 2);
        label.setSize(18, 18);
        panelAddressees.add(label);

        textCc = new JTextField();
        textCc.setLocation(getRight(label) + GAP, getBottom(textTo) + GAP);
        textCc.setSize(textTo.getSize());
        textCc.setEditable(false);
        panelAddressees.add(textCc);

        buttonCc = new JButton("...");
        buttonCc.setMargin(new Insets(0, 0, 0, 0));
        buttonCc.setSize(25, 25);
        buttonCc.setLocation(buttonTo.getX(), textCc.getY());
        buttonCc.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonCcClick();
            }
        });
        panelAddressees.add(buttonCc);

        label = new JLabel("Bcc:");
        label.setLocation(areaPanel.left + GAP, getBottom(textCc) + GAP + 2);
        label.setSize(25, 18);
        panelAddressees.add(label);

        textBcc = new JTextField();
        textBcc.setLocation(getRight(label) + GAP, getBottom(textCc) + GAP);
        textBcc.setSize(textTo.getSize());
        textBcc.setEditable(false);
        panelAddressees.add(textBcc);

        buttonBcc = new JButton("...");
        buttonBcc.setMargin(new Insets(0, 0, 0, 0));
        buttonBcc.setSize(25, 25);
        buttonBcc.setLocation(buttonTo.getX(), textBcc.getY());
        buttonBcc.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonBccClick();
            }
        });
        panelAddressees.add(buttonBcc);

        // Options groupbox

        panelOptions = new JPanel(null);
        panelOptions.setBorder(BorderFactory.createTitledBorder(" Options "));
        panelOptions.setLocation(GAP, getBottom(panelAddressees) + GAP);
        panelOptions.setSize(panelAddressees.getWidth(), 96);
        add(panelOptions);
        areaPanel = getClientArea(panelOptions);

        label = new JLabel("Subject:");
        label.setLocation(areaPanel.left + GAP, areaPanel.top + GAP + 2);
        label.setSize(46, 18);
        panelOptions.add(label);

        textSubject = new JTextField();
        textSubject.setLocation(getRight(label) + GAP, areaPanel.top + GAP);
        textSubject.setSize(areaPanel.right - getRight(label) - DGAP, 25);
        panelOptions.add(textSubject);

        label = new JLabel("Priority:");
        label.setLocation(areaPanel.left + GAP + 2, getBottom(textSubject) + GAP + 2);
        label.setSize(44, 18);
        panelOptions.add(label);

        comboPriority = new JComboBox(PRIORITIES);
        comboPriority.setLocation(textSubject.getX(), getBottom(textSubject) + GAP);
        comboPriority.setSize(70, 25);
        comboPriority.setSelectedIndex(2);  // Normal priority
        panelOptions.add(comboPriority);

        checkDeliveryReceipt = new JCheckBox("Req. delivery receipt");
        checkDeliveryReceipt.setLocation(getRight(comboPriority) + DGAP, getBottom(textSubject) + GAP + 2);
        checkDeliveryReceipt.setSize(140, 20);
        panelOptions.add(checkDeliveryReceipt);

        checkReadReceipt = new JCheckBox("Req. read receipt");
        checkReadReceipt.setLocation(getRight(checkDeliveryReceipt) + DGAP, checkDeliveryReceipt.getY());
        checkReadReceipt.setSize(122, 20);
        panelOptions.add(checkReadReceipt);

        // Security groupbox

        panelSecurity = new JPanel(null);
        panelSecurity.setBorder(BorderFactory.createTitledBorder(" Security "));
        panelSecurity.setLocation(GAP, getBottom(panelOptions) + GAP);
        panelSecurity.setSize(panelOptions.getWidth(), areaFrame.bottom - areaFrame.top - getBottom(panelOptions) - DGAP);
        add(panelSecurity);
        areaPanel = getClientArea(panelSecurity);

        label = new JLabel("Signing certificate (should have an associated private key):");
        label.setLocation(areaPanel.left + GAP, areaPanel.top + GAP);
        label.setSize(areaPanel.right - areaPanel.left - DGAP, 18);
        panelSecurity.add(label);

        textSigningCertificate = new JTextField();
        textSigningCertificate.setLocation(label.getX(), getBottom(label) + HGAP);
        textSigningCertificate.setEditable(false);
        panelSecurity.add(textSigningCertificate);

        buttonLoadSigningCertificate = new JButton("...");
        buttonLoadSigningCertificate.setMargin(new Insets(0, 0, 0, 0));
        buttonLoadSigningCertificate.setSize(25, 25);
        buttonLoadSigningCertificate.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonLoadSigningCertificateClick();
            }
        });
        panelSecurity.add(buttonLoadSigningCertificate);

        buttonClearSigningCertificate = new JButton("X");
        buttonClearSigningCertificate.setMargin(new Insets(0, 0, 0, 0));
        buttonClearSigningCertificate.setSize(25, 25);
        buttonClearSigningCertificate.setLocation(areaPanel.right - GAP - 25, getBottom(label) + HGAP);
        buttonClearSigningCertificate.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonClearSigningCertificateClick();
            }
        });
        panelSecurity.add(buttonClearSigningCertificate);

        buttonLoadSigningCertificate.setLocation(buttonClearSigningCertificate.getX() - buttonLoadSigningCertificate.getWidth() - GAP,
                buttonClearSigningCertificate.getY());
        textSigningCertificate.setSize(buttonLoadSigningCertificate.getX() - textSigningCertificate.getX() - GAP, 25);

        label = new JLabel("Hash algorithm:");
        label.setLocation(areaPanel.left + GAP, getBottom(textSigningCertificate) + GAP + 2);
        label.setSize(89, 18);
        panelSecurity.add(label);

        comboHashAlgorithm = new JComboBox(HASH_ALGORITHM_NAMES);
        comboHashAlgorithm.setLocation(getRight(label) + GAP, getBottom(textSigningCertificate) + GAP);
        comboHashAlgorithm.setSize(100, 25);
        comboHashAlgorithm.setSelectedIndex(3); // SHA-256
        panelSecurity.add(comboHashAlgorithm);

        label = new JLabel("Signature format:");
        label.setLocation(getRight(comboHashAlgorithm) + DGAP, getBottom(textSigningCertificate) + GAP + 2);
        label.setSize(99, 18);
        panelSecurity.add(label);

        comboSignatureFormat = new JComboBox(SIGNATURE_FORMATS);
        comboSignatureFormat.setLocation(getRight(label) + GAP, getBottom(textSigningCertificate) + GAP);
        comboSignatureFormat.setSize(100, 25);
        panelSecurity.add(comboSignatureFormat);

        label = new JLabel("Encryption certificates (1 cert for each addressee, private keys not needed):");
        label.setLocation(areaPanel.left + GAP, getBottom(comboHashAlgorithm) + TGAP);
        label.setSize(areaPanel.right - areaPanel.left - DGAP, 18);
        panelSecurity.add(label);

        buttonDeleteEncryptionCertificate = new JButton("Delete");
        buttonDeleteEncryptionCertificate.setSize(75, 25);
        buttonDeleteEncryptionCertificate.setLocation(areaPanel.right - buttonDeleteEncryptionCertificate.getWidth() - GAP,
                areaPanel.bottom - buttonDeleteEncryptionCertificate.getHeight() - GAP);
        buttonDeleteEncryptionCertificate.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonDeleteEncryptionCertificateClick();
            }
        });

        buttonLoadEncryptionCertificate = new JButton("Load...");
        buttonLoadEncryptionCertificate.setSize(75, 25);
        buttonLoadEncryptionCertificate.setLocation(buttonDeleteEncryptionCertificate.getX() - buttonLoadEncryptionCertificate.getWidth() - GAP,
                buttonDeleteEncryptionCertificate.getY());
        buttonLoadEncryptionCertificate.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonLoadEncryptionCertificateClick();
            }
        });

        scrollEncryptionCertificates = new JScrollPane();
        scrollEncryptionCertificates.setLocation(areaPanel.left + GAP, getBottom(label) + HGAP);
        scrollEncryptionCertificates.setSize(areaPanel.right - areaPanel.left - DGAP,
                buttonDeleteEncryptionCertificate.getY() - getBottom(label) - DGAP);
        modelEncryptionCertificates = new ReadOnlyTableModel(ENCRYPTION_CERTIFICATE_COLUMNS);
        tableEncryptionCertificates = new JTable(modelEncryptionCertificates);
        scrollEncryptionCertificates.setViewportView(tableEncryptionCertificates);
        panelSecurity.add(scrollEncryptionCertificates);

        panelSecurity.add(buttonLoadEncryptionCertificate);
        panelSecurity.add(buttonDeleteEncryptionCertificate);

        label = new JLabel("Encryption algorithm:");
        label.setLocation(scrollEncryptionCertificates.getX(), buttonLoadEncryptionCertificate.getY() + 2);
        label.setSize(120, 18);
        panelSecurity.add(label);

        comboEncryptionAlgorithm = new JComboBox<>(ENCRYPTION_ALGORITHM_NAMES);
        comboEncryptionAlgorithm.setLocation(getRight(label) + GAP, buttonLoadEncryptionCertificate.getY());
        comboEncryptionAlgorithm.setSize(100, 25);
        comboEncryptionAlgorithm.setSelectedIndex(2);   // AES 128
        panelSecurity.add(comboEncryptionAlgorithm);

        // Right side groupboxes (pre-created)

        panelText = new JPanel(null);
        panelText.setBorder(BorderFactory.createTitledBorder(" Text "));
        add(panelText);

        panelAttachments = new JPanel(null);
        panelAttachments.setBorder(BorderFactory.createTitledBorder(" Attachments "));
        add(panelAttachments);

        // Assemble and Save groupbox

        panelSave = new JPanel(null);
        panelSave.setBorder(BorderFactory.createTitledBorder(" Assemble and Save "));
        panelSave.setSize(areaFrame.right - areaFrame.left - getRight(panelSecurity) - DGAP, 100);
        panelSave.setLocation(getRight(panelSecurity) + GAP, areaFrame.bottom - areaFrame.top - GAP - panelSave.getHeight());
        add(panelSave);
        areaPanel = getClientArea(panelSave);

        label = new JLabel("Filename:");
        label.setLocation(areaPanel.left + GAP, areaPanel.top + GAP + 2);
        label.setSize(54, 18);
        panelSave.add(label);

        textFilename = new JTextField();
        textFilename.setLocation(getRight(label) + GAP, areaPanel.top + GAP);
        panelSave.add(textFilename);

        buttonFilename = new JButton("...");
        buttonFilename.setMargin(new Insets(0, 0, 0, 0));
        buttonFilename.setSize(25, 25);
        buttonFilename.setLocation(areaPanel.right - GAP - buttonFilename.getWidth(), areaPanel.top + GAP);
        buttonFilename.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonFilenameClick();
            }
        });
        panelSave.add(buttonFilename);

        textFilename.setSize(buttonFilename.getX() - getRight(label) - DGAP, 25);

        buttonSave = new JButton("Assemble and Save");
        buttonSave.setSize(220, 25);
        buttonSave.setLocation(areaPanel.left + (areaPanel.right - areaPanel.left - buttonSave.getWidth()) / 2,
                getBottom(textFilename) + DGAP);
        buttonSave.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonSaveClick();
            }
        });
        panelSave.add(buttonSave);

        // Attachments groupbox

        panelAttachments.setSize(areaFrame.right - areaFrame.left - getRight(panelSecurity) - DGAP, 150);
        panelAttachments.setLocation(panelSave.getX(), panelSave.getY() - panelAttachments.getHeight() - GAP);
        areaPanel = getClientArea(panelAttachments);

        buttonDeleteAttachment = new JButton("Delete");
        buttonDeleteAttachment.setSize(80, 25);
        buttonDeleteAttachment.setLocation(areaPanel.right - buttonDeleteAttachment.getWidth() - GAP,
                areaPanel.bottom - buttonDeleteAttachment.getHeight() - GAP);
        buttonDeleteAttachment.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonDeleteAttachmentClick();
            }
        });

        buttonAttach = new JButton("Attach...");
        buttonAttach.setSize(80, 25);
        buttonAttach.setLocation(buttonDeleteAttachment.getX() - buttonAttach.getWidth() - GAP, buttonDeleteAttachment.getY());
        buttonAttach.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonAttachClick();
            }
        });

        scrollAttachments = new JScrollPane();
        scrollAttachments.setLocation(areaPanel.left + GAP, areaPanel.top + GAP);
        scrollAttachments.setSize(areaPanel.right - areaPanel.left - DGAP, buttonAttach.getY() - areaPanel.top - DGAP);
        modelAttachments = new ReadOnlyTableModel(ATTACHMENT_COLUMNS);
        tableAttachments = new JTable(modelAttachments);
        tableAttachments.getColumnModel().getColumn(0).setPreferredWidth((int)(scrollAttachments.getWidth() * 0.5));
        tableAttachments.getColumnModel().getColumn(1).setPreferredWidth((int)(scrollAttachments.getWidth() * 0.35));
        tableAttachments.getColumnModel().getColumn(2).setPreferredWidth((int)(scrollAttachments.getWidth() * 0.15));
        tableAttachments.getColumnModel().getColumn(2).setCellRenderer(new AttachmentSizeRenderer());
        scrollAttachments.setViewportView(tableAttachments);
        panelAttachments.add(scrollAttachments);

        panelAttachments.add(buttonAttach);
        panelAttachments.add(buttonDeleteAttachment);

        // Text groupbox

        panelText.setSize(panelAttachments.getWidth(), panelAttachments.getY() - areaFrame.top - DGAP);
        panelText.setLocation(getRight(panelOriginators) + GAP, panelOriginators.getY());
        areaPanel = getClientArea(panelText);

        buttonEmbed = new JButton("Embed...");
        buttonEmbed.setMargin(new Insets(0, 0, 0, 0));
        buttonEmbed.setSize(70, 25);
        buttonEmbed.setLocation(areaPanel.right - buttonEmbed.getWidth() - GAP,
                areaPanel.bottom - buttonEmbed.getHeight() - GAP);
        buttonEmbed.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                buttonEmbedClick();
            }
        });

        label = new JLabel("Plain text:");
        label.setLocation(areaPanel.left + GAP, areaPanel.top + HGAP);
        label.setSize(56, 18);
        panelText.add(label);

        JScrollPane scroller = new JScrollPane();
        scroller.setLocation(label.getX(), getBottom(label) + HGAP);
        scroller.setSize(areaPanel.right - areaPanel.left - DGAP,
                (buttonEmbed.getY() - getBottom(label) - TGAP - label.getHeight()) / 2);
        textPlain = new JTextArea();
        scroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scroller.setViewportView(textPlain);
        panelText.add(scroller);

        label = new JLabel("HTML text:");
        label.setLocation(areaPanel.left + GAP, getBottom(scroller) + GAP);
        label.setSize(60, 18);
        panelText.add(label);

        scroller = new JScrollPane();
        scroller.setLocation(label.getX(), getBottom(label) + HGAP);
        scroller.setSize(areaPanel.right - areaPanel.left - DGAP,
                buttonEmbed.getY() - getBottom(label) - GAP - HGAP);
        textHtml = new JTextArea();
        scroller.setViewportView(textHtml);
        scroller.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scroller.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        panelText.add(scroller);

        panelText.add(buttonEmbed);

        label = new JLabel("<html><p align=\"center\">(if the HTML text contains references to images that need to be embedded into the message, use the button to the right to add them)</p></html>");
        label.setLocation(scroller.getX(), getBottom(scroller) + GAP);
        label.setSize(buttonEmbed.getX() - areaPanel.left - DGAP, buttonEmbed.getHeight());
        font = label.getFont();
        font = font.deriveFont(Font.PLAIN, (float)(font.getSize() - 1));
        label.setFont(font);
        label.setForeground(SystemColor.textInactiveText);
        panelText.add(label);
    }

    private Insets getClientArea(JFrame f) {
        Rectangle b = f.getBounds();
        Insets i = f.getInsets();
        return new Insets(i.top, i.left,b.height - i.bottom,b.width - i.right);
    }

    private Insets getClientArea(JComponent c) {
        Rectangle b = c.getBounds();
        Insets i = c.getInsets();
        return new Insets(i.top, i.left,b.height - i.bottom,b.width - i.right);
    }

    static int getBottom(JComponent c) {
        return c.getY() + c.getHeight();
    }

    static int getRight(JComponent c) {
        return c.getX() + c.getWidth();
    }

    private void applyContentType(MailAttachment attachment) {
        try {
            attachment.setContentType(URLConnection.guessContentTypeFromName(attachment.getFilename()));
        }
        catch (SecureBlackboxException err) {
            //
        }
    }

    private static byte[] loadFile(String filename) throws IOException {
        try (RandomAccessFile file = new RandomAccessFile(filename, "r")) {
            byte[] buffer = new byte[(int) file.length()];
            file.readFully(buffer);
            return buffer;
        }
    }

    private static String getExtension(String filename) {
        if (filename == null)
            return "";

        int pos = filename.lastIndexOf('.');
        if (pos < 0 || pos == filename.length() - 1)
            return "";

        return filename.substring(pos + 1);
    }

    private JPanel panelOriginators;
    private JTextField textFrom;
    private JButton buttonFrom;
    private JTextField textSender;

    private JPanel panelAddressees;
    private JTextField textTo;
    private JButton buttonTo;
    private JTextField textCc;
    private JButton buttonCc;
    private JTextField textBcc;
    private JButton buttonBcc;

    private JPanel panelOptions;
    private JTextField textSubject;
    private JComboBox comboPriority;
    private JCheckBox checkDeliveryReceipt;
    private JCheckBox checkReadReceipt;

    private JPanel panelSecurity;
    private JTextField textSigningCertificate;
    private JButton buttonLoadSigningCertificate;
    private JButton buttonClearSigningCertificate;
    private JComboBox comboHashAlgorithm;
    private JComboBox comboSignatureFormat;
    private JScrollPane scrollEncryptionCertificates;
    private DefaultTableModel modelEncryptionCertificates;
    private JTable tableEncryptionCertificates;
    private JComboBox comboEncryptionAlgorithm;
    private JButton buttonLoadEncryptionCertificate;
    private JButton buttonDeleteEncryptionCertificate;

    private JPanel panelText;
    private JTextArea textPlain;
    private JTextArea textHtml;
    private JButton buttonEmbed;

    private JPanel panelAttachments;
    private JScrollPane scrollAttachments;
    private DefaultTableModel modelAttachments;
    private JTable tableAttachments;
    private JButton buttonAttach;
    private JButton buttonDeleteAttachment;

    private JPanel panelSave;
    private JTextField textFilename;
    private JButton buttonFilename;
    private JButton buttonSave;

    static final int GAP = 6;
    static final int HGAP = GAP / 2;
    static final int DGAP = GAP * 2;
    static final int TGAP = GAP * 3;

    private static final String[] PRIORITIES = { "Lowest", "Low", "Normal", "High", "Highest" };
    private static final String[] HASH_ALGORITHM_NAMES = {
            "MD5 (legacy)", "SHA-1 (legacy)", "SHA-224", "SHA-256", "SHA-384", "SHA-512",
            "SHA3-224", "SHA3-256", "SHA3-384", "SHA3-512"
    };
    private static final String[] HASH_ALGORITHM_CONSTS = {
            Constants.SB_HASH_ALGORITHM_MD5, Constants.SB_HASH_ALGORITHM_SHA1,
            Constants.SB_HASH_ALGORITHM_SHA224, Constants.SB_HASH_ALGORITHM_SHA256,
            Constants.SB_HASH_ALGORITHM_SHA384, Constants.SB_HASH_ALGORITHM_SHA512,
            Constants.SB_HASH_ALGORITHM_SHA3_224, Constants.SB_HASH_ALGORITHM_SHA3_256,
            Constants.SB_HASH_ALGORITHM_SHA3_384, Constants.SB_HASH_ALGORITHM_SHA3_512
    };
    private static final String[] SIGNATURE_FORMATS = { "multipart/signed", "signed-data" };
    private static final String[] ENCRYPTION_ALGORITHM_NAMES = {
            "DES (legacy)", "Tripple DES", "AES 128", "AES 192", "AES 256",
            "Blowfish", "TwoFish", "Camellia", "Serpent"
    };
    private static final String[] ENCRYPTION_ALGORITHM_CONSTS = {
            Constants.SB_SYMMETRIC_ALGORITHM_DES, Constants.SB_SYMMETRIC_ALGORITHM_3DES,
            Constants.SB_SYMMETRIC_ALGORITHM_AES128, Constants.SB_SYMMETRIC_ALGORITHM_AES192,
            Constants.SB_SYMMETRIC_ALGORITHM_AES256, Constants.SB_SYMMETRIC_ALGORITHM_BLOWFISH,
            Constants.SB_SYMMETRIC_ALGORITHM_TWOFISH, Constants.SB_SYMMETRIC_ALGORITHM_CAMELLIA,
            Constants.SB_SYMMETRIC_ALGORITHM_SERPENT
    };

    private static final String[] ATTACHMENT_COLUMNS = { "Filename/ID", "Content type", "Size" };
    private static final String[] ENCRYPTION_CERTIFICATE_COLUMNS = { "Subject", "Issuer" };

    private static class ReadOnlyTableModel extends DefaultTableModel {
        ReadOnlyTableModel(String[] columns) {
            super(columns, 0);
        }

        public boolean isCellEditable(int row, int col) {
            return false;
        }
    }

    private static class AttachmentSizeRenderer extends DefaultTableCellRenderer {
        private static final DecimalFormat formatter = new DecimalFormat("####,###");

        AttachmentSizeRenderer() {
            super();
            setHorizontalAlignment(SwingConstants.RIGHT);
        }

        public Component getTableCellRendererComponent(JTable table, Object value, boolean selected, boolean focused, int row, int column) {
            value = formatter.format((Number)value);
            return super.getTableCellRendererComponent(table, value, selected, focused, row, column);
        }
    }
}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof SecureBlackboxException) {
      System.out.print(" (" + ((SecureBlackboxException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



