/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.headless;

import jp.oist.flint.form.IFrame;

class Logger implements IFrame {

    public void printError(String message) {
        // Print an error message to stderr directly by design,
        // because it should notify users of errors on a console.
        System.err.println(message);
    }

    @Override
    public void appendLog(String s) {
        printError(s);
    }

    @Override
    public void showErrorDialog(String message, String title) {
        printError(title + " | " + message);
    }
}
