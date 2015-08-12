/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.export.ExportWorker;
import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.form.IFrame;
import jp.oist.flint.form.sub.JobWindow;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import javax.swing.ProgressMonitor;
import javax.swing.SwingWorker;

public class ExportAllWorker extends SwingWorker<Boolean, Void> {

    private final IFrame mFrame;

    private final ProgressMonitor mMonitor;

    private final String mExtension;

    private final File mListFile;

    private final List<File> mSourceFiles;

    private final List<File> mTargetFiles;

    private final List<Map<String, Number>> mParameters;

    public ExportAllWorker(IFrame frame,
                           JobWindow window,
                           String extension,
                           File listFile,
                           List<File> sourceFiles, List<File> targetFiles,
                           List<Map<String, Number>> parameters) {
        mFrame = frame;
        int size = sourceFiles.size();
        assert size == targetFiles.size();
        assert size == parameters.size();
        mMonitor = new ProgressMonitor(window,
                                       "Exporting ...",
                                       null,
                                       0,
                                       size);
        assert extension != null;
        mExtension = extension;
        mListFile = listFile;
        mSourceFiles = sourceFiles;
        mTargetFiles = targetFiles;
        mParameters = parameters;
    }

    @Override
    protected Boolean doInBackground()
        throws ExecutionException, IOException, InterruptedException {
        int size = mSourceFiles.size();
        try (FileOutputStream fos = new FileOutputStream(mListFile);
             OutputStreamWriter osw = new OutputStreamWriter(fos, StandardCharsets.UTF_8);
             BufferedWriter writer = new BufferedWriter(osw)) {
            for (int i=0;i<size;i++) {
                if (mMonitor.isCanceled())
                    return true;

                File sourceFile = mSourceFiles.get(i);
                File targetFile = mTargetFiles.get(i);
                switch (mExtension) {
                case "csv":
                    {
                        ExportWorker worker = new ExportWorker(mFrame, sourceFile, targetFile);
                        worker.execute();
                        if (!worker.get())
                            return false;
                    }
                    break;
                default: // isd
                    Workspace.publishFile(sourceFile, targetFile);
                    break;
                }

                String line = createLine(targetFile, mParameters.get(i));
                writer.write(line);
                writer.newLine();

                mMonitor.setProgress(i+1);
            }
        }
        return true;
    }

    @Override
    protected void done() {
        mMonitor.close();
    }

    private String createLine(File file, Map<String, Number> map) {
        StringBuilder sb = new StringBuilder(file.getName());
        for (Map.Entry<String, Number> entry : map.entrySet()) {
            sb.append(",");
            sb.append(entry.getKey());
            sb.append("=");
            sb.append(entry.getValue());
        }
        return sb.toString();
    }
}
