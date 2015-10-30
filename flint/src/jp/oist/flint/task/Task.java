/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.task;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class Task {

    private final int mId;

    private final ArrayList<ParameterDefinition> mParameterDefinitions;

    public Task(int id, File workingDir)
        throws IOException, ParameterDefinitionException {
        mId = id;

        ArrayList<ParameterDefinition> parameterDefinitions = new ArrayList<>();
        File file = new File(workingDir, "parameters.txt");
        try (Scanner scanner = new Scanner(file, "UTF8")) {
            while (scanner.hasNextLine()) {
                parameterDefinitions.add(new ParameterDefinition(scanner));
            }
        }
        if (parameterDefinitions.isEmpty())
            throw new ParameterDefinitionException("no parameters found: " + file.getPath());
        mParameterDefinitions = parameterDefinitions;
    }

    public int getId() {
        return mId;
    }

    public int getNumberOfParameters() {
        return mParameterDefinitions.size();
    }

    public ParameterDefinition getParameterDefinition(int i) {
        return mParameterDefinitions.get(i);
    }

    public int getNumberOfJobs() {
        int p = 1;
        for (ParameterDefinition pd : mParameterDefinitions) {
            p *= pd.getNumberOfValues();
        }
        return p;
    }
}
