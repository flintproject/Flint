/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ParameterComboBoxActionListener implements ActionListener {

    private final TaskWindow mWindow;

    public ParameterComboBoxActionListener(TaskWindow window) {
        mWindow = window;
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        mWindow.changeCell();
    }
}
