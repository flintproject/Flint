/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.BorderLayout;
import java.awt.Container;
import javax.swing.JPanel;

abstract class PeripheralPane extends JPanel {

    public PeripheralPane() {
        super(new BorderLayout());
    }

    public void setContentPane(Container contentPane) {
        add(contentPane);
    }
}
