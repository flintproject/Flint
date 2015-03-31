/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import jp.oist.flint.form.IFrame;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.concurrent.ExecutionException;
import javax.swing.SwingWorker;

public class ProcessWorker extends SwingWorker<Integer, String> {

    private final String mCommand;
    private final Process mProcess;
    private final IFrame mFrame;

    public ProcessWorker(String command, Process process, IFrame frame) {
        mCommand = command;
        mProcess = process;
        mFrame = frame;
    }

    @Override
    protected Integer doInBackground() throws IOException, InterruptedException {
        // This takes care of a process's stdout, because the process is
        // supposed to redirect its stderr to its stdout.
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(mProcess.getInputStream()))) {
            String line;
            for (;;) {
                line = reader.readLine();
                if (line == null) {
                    break;
                } else {
                    publish(line);
                }
            }
            return mProcess.waitFor();
        }
    }

    @Override
    protected void process(List<String> lines) {
        for (String line : lines) {
            mFrame.appendLog(line);
        }
    }

    @Override
    protected void done() {
        Integer result;
        try {
            result = get();
        } catch (ExecutionException ee) {
            mFrame.showErrorDialog(ee.getMessage(), "A process execution failed");
            return;
        } catch (InterruptedException ie) {
            mFrame.showErrorDialog(ie.getMessage(), "A process interrupted");
            return;
        }
        int r = result.intValue();
        if (r == 0) {
            // nothing to do
        } else {
            mFrame.showErrorDialog("A process exited abnormally: " + r, "A process exited abnormally");
        }
    }
}
