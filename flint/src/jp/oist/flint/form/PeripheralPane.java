/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.form;

import java.awt.BorderLayout;
import java.awt.Container;
import javax.swing.JPanel;

public abstract class PeripheralPane extends JPanel {

    private Container mContentPane;

    private String mTitle;

    public PeripheralPane (String title) {
        super(new BorderLayout());

        mTitle = title;

        initComponents();
    }

    private void initComponents () {
        mContentPane = new JPanel();

        add(mContentPane, BorderLayout.CENTER);
    }

    public void setContentPane (Container contentPane) {
        remove(mContentPane);
        mContentPane = contentPane;
        add(mContentPane);
    }

    public Container getContentPane () {
        return mContentPane;
    }

    public void setTitle (String title) {
        mTitle = title;
    }

    public String getTitle () {
        return mTitle;
    }
}
