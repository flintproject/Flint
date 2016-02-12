/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.desktop.Desktop;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.util.ArrayList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

public class ModelLoaderLogger implements IFrame {

    private final ArrayList<String> mLines = new ArrayList<>();

    private final Desktop mDesktop;

    public ModelLoaderLogger(Desktop desktop) {
        mDesktop = desktop;
    }

    public Desktop getDesktop() {
        return mDesktop;
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
        final JScrollPane scrollPane = new JScrollPane(textArea);
        // Trick for a resizeable JOptionPane dialog from:
        // <https://blogs.oracle.com/scblog/entry/tip_making_joptionpane_dialog_resizable>
        scrollPane.addHierarchyListener(new HierarchyListener() {
            @Override
            public void hierarchyChanged(HierarchyEvent e) {
                Window window = SwingUtilities.getWindowAncestor(scrollPane);
                if (window instanceof Dialog) {
                    Dialog dialog = (Dialog)window;
                    if (!dialog.isResizable())
                        dialog.setResizable(true);
                }
            }
        });
        scrollPane.setPreferredSize(new Dimension(400, 100));
        JOptionPane.showMessageDialog(mDesktop.getPane(), scrollPane, title, JOptionPane.ERROR_MESSAGE);
    }
}
