/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.BorderLayout;
import java.awt.Component;
import javax.swing.Icon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class MessageDialog extends JPanel {

    public static void showMessageDialog (Component parent, Object msg, Object detail, String title) {
        showMessageDialog(parent, msg, detail, title, JOptionPane.INFORMATION_MESSAGE, null, new Object[] {"CANCEL", "OK"});
    }

    public static void showMessageDialog (Component parent, Object msg, Object detail, String title, int messageType) {
        showMessageDialog(parent, msg, detail, title, messageType, null, new Object[]{"CANCEL", "OK"});
    }

    public static void showMessageDialog (Component parent, Object msg, Object detail, String title, int messageType, Icon icon) {
        showMessageDialog(parent, msg, detail, title, messageType, icon, new Object[]{"CANCEL", "OK"});
    }

    public static void showMessageDialog (Component parent, Object msg, Object detail, String title, int messageType, Icon icon, Object[] options) {
        MessageDialog me = new MessageDialog(msg, detail);
        JOptionPane optionPane = new JOptionPane(me, messageType, 
                JOptionPane.DEFAULT_OPTION, icon, options);

        if (parent != null)
            optionPane.setComponentOrientation(parent.getComponentOrientation());

        JDialog dialog = optionPane.createDialog(parent, title);
        dialog.pack();
        dialog.setResizable(true);

        dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        dialog.setVisible(true);
    }

    private final String mMessage;

    private final String mDetail;

    private MessageDialog (Object message, Object detail) {
        super(new BorderLayout());

        mMessage = (message == null)? "" : message.toString();
        mDetail  = (detail == null)? "" : detail.toString();

        initComponents();
    }

    private void initComponents () {
        JLabel messageLabel = new JLabel(mMessage);

        JTextArea detailArea = new JTextArea(mDetail);
        detailArea.setEditable(false);

        add(messageLabel, BorderLayout.NORTH);
        add(new JScrollPane(detailArea), BorderLayout.CENTER);
    }

}
