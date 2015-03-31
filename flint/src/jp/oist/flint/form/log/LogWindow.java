/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.log;

import java.awt.BorderLayout;
import java.awt.Window;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class LogWindow extends JDialog {

    private JTextArea mTextArea;

    public LogWindow (Window parent, String title) {
        super(parent, title, JDialog.ModalityType.APPLICATION_MODAL);
        initComponents();
    }

    private void initComponents () {
        JPanel contentPane = createContentPane();
        contentPane.setLayout(new BorderLayout());
        setContentPane(contentPane);

        mTextArea =  new JTextArea();
        mTextArea.setEditable(false);
        mTextArea.setColumns(40);
        mTextArea.setRows(20);
        mTextArea.setTabSize(4);
        mTextArea.setDoubleBuffered(true);

        JScrollPane scrollPane = new JScrollPane(mTextArea);
        scrollPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5,5));

        add(scrollPane, BorderLayout.CENTER);

        pack();
    }

    private JPanel createContentPane () {
        return new JPanel();
    }

    public void appendText (String text) {
        mTextArea.append(text);
    }

    public void setText (String text) {
        mTextArea.setText(text);
    }

    public String getText () {
        return mTextArea.getText();
    }

    public void cut () {
        mTextArea.selectAll();
        mTextArea.setEnabled(true);
        mTextArea.cut();
        mTextArea.setEnabled(false);
    }

    public void copy () {
        mTextArea.selectAll();
        mTextArea.copy();
    }
}
