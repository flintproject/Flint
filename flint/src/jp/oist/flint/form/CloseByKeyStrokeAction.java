/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.Component;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

public class CloseByKeyStrokeAction extends AbstractAction {

    private final Window mWindow;

    private final Component mOwner; // focus moves on this after closing window

    public CloseByKeyStrokeAction(Window window, Component owner) {
        mWindow = window;
        mOwner = owner;
    }

    @Override
    public void actionPerformed(ActionEvent event) {
        mWindow.setVisible(false);
        mOwner.requestFocus();
        mWindow.dispose();
    }

    private static CloseByKeyStrokeAction register(Window window, JComponent component, Component owner) {
        CloseByKeyStrokeAction action = new CloseByKeyStrokeAction(window, owner);
        InputMap inputMap = component.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "close");
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "close");
        component.getActionMap().put("close", action);
        return action;
    }

    public static CloseByKeyStrokeAction register(JDialog dialog) {
        return register(dialog, dialog.getRootPane(), dialog.getOwner());
    }

    public static CloseByKeyStrokeAction register(JFrame frame, Component owner) {
        return register(frame, frame.getRootPane(), owner);
    }

}
