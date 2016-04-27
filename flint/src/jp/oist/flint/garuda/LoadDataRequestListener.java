/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.form.MainFrame;
import jp.sbi.garuda.backend.incomingHandler.garudaActionListeners.requests.LoadDataRequestActionListener;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

class LoadDataRequestListener
    implements LoadDataRequestActionListener {

    private final MainFrame mFrame;

    public LoadDataRequestListener(MainFrame frame) {
        mFrame = frame;
    }

    @Override
    public void loadDataRequestReceivedAsFile(File file, String senderId, String senderName) {
        mFrame.openModel(file);
    }

    @Override
    public void loadDataRequestReceivedAsStream(InputStream in, String senderId, String senderName) {
        try {
            File file = Workspace.createTempFile("garuda", "");
            Files.copy(in, file.toPath());
            loadDataRequestReceivedAsFile(file, senderId, senderName);
        } catch (IOException ioe) {
            reportError(ioe.getMessage());
        }
    }

    private void reportError(final String message) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                JOptionPane.showMessageDialog(mFrame,
                                              message,
                                              "Error on loading data",
                                              JOptionPane.ERROR_MESSAGE);
            }
        });
    }
}
