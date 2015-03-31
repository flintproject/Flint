/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import javax.swing.SwingWorker;

public class ProcessWatcher extends SwingWorker<Void, String> {

    private final InputStream mStream;

    public ProcessWatcher (InputStream stream) {
        mStream = stream;
    }

    @Override
    protected Void doInBackground() throws IOException {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(mStream))) {
            while (true) {
                String line = br.readLine();
                if (line == null)
                    return null;
                publish(line);
            }
        }
    }
}
