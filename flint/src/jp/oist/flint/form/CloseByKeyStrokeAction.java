/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

public class CloseByKeyStrokeAction extends AbstractAction {

    private final Window mWindow;

    public CloseByKeyStrokeAction(Window window) {
        mWindow = window;
    }

    @Override
    public void actionPerformed(ActionEvent event) {
        mWindow.dispose();
    }

    private static void register(Window window, JComponent component) {
        CloseByKeyStrokeAction action = new CloseByKeyStrokeAction(window);
        InputMap inputMap = component.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "close");
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_W, InputEvent.CTRL_DOWN_MASK), "close");
        component.getActionMap().put("close", action);
    }

    public static void register(JDialog dialog) {
        register(dialog, dialog.getRootPane());
    }

    public static void register(JFrame frame) {
        register(frame, frame.getRootPane());
    }

}
