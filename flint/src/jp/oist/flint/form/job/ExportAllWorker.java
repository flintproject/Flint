/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import javax.swing.SwingWorker;
import jp.oist.flint.filesystem.Workspace;

public class ExportAllWorker extends SwingWorker<Void, Void> {

    private final File mListFile;

    private final List<File> mSourceFiles;

    private final List<File> mTargetFiles;

    private final List<Map<String, Number>> mParameters;

    public ExportAllWorker(File listFile,
                           List<File> sourceFiles, List<File> targetFiles,
                           List<Map<String, Number>> parameters) {
        mListFile = listFile;
        assert sourceFiles.size() == targetFiles.size();
        assert sourceFiles.size() == parameters.size();
        mSourceFiles = sourceFiles;
        mTargetFiles = targetFiles;
        mParameters = parameters;
    }

    @Override
    protected Void doInBackground() throws IOException {
        int size = mSourceFiles.size();
        try (FileOutputStream fos = new FileOutputStream(mListFile);
             OutputStreamWriter osw = new OutputStreamWriter(fos, StandardCharsets.UTF_8);
             BufferedWriter writer = new BufferedWriter(osw)) {
            for (int i=0;i<size;i++) {
                File sourceFile = mSourceFiles.get(i);
                File targetFile = mTargetFiles.get(i);
                Workspace.publishFile(sourceFile, targetFile);

                String line = createLine(targetFile, mParameters.get(i));
                writer.write(line);
                writer.newLine();

                int progress = (int)((double)i / (double)size * 100.0);
                setProgress(progress);
            }
        }
        return null;
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
