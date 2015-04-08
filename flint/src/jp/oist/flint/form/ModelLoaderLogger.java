/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import com.google.protobuf.ByteString;
import java.awt.Dimension;
import java.util.ArrayList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class ModelLoaderLogger implements IFrame {

    private final ArrayList<String> mLines = new ArrayList<>();

    private final MainFrame mFrame;

    public ModelLoaderLogger(MainFrame frame) {
        mFrame = frame;
    }

    public MainFrame getFrame() {
        return mFrame;
    }

    @Override
    public void appendLog(String s) {
        mLines.add(s);
    }

    @Override
    public void showErrorDialog(String message, String title) {
        JTextArea textArea = new JTextArea(message);
        textArea.setEditable(false);
        textArea.append(System.getProperty("line.separator"));
        for (String line : mLines) {
            textArea.append(System.getProperty("line.separator"));
            textArea.append(line);
        }
        JScrollPane scrollPane = new JScrollPane(textArea);
        scrollPane.setPreferredSize(new Dimension(400, 100));
        JOptionPane.showMessageDialog((java.awt.Frame)mFrame, scrollPane, title, JOptionPane.ERROR_MESSAGE);
    }

    @Override
    public void showErrorDialog(ByteString message, String title) {
        showErrorDialog(message.toStringUtf8(), title);
    }
}
