/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.task;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

/*
 * This represents a parameter and its possible values for a task.
 */
public class ParameterDefinition {

    private final String mName;

    private final ArrayList<Double> mValues;

    public ParameterDefinition(Scanner scanner)
        throws IOException, ParameterDefinitionException {
        String line = scanner.nextLine();
        String[] s = line.split("\\|");
        if (s.length != 2)
            throw new ParameterDefinitionException("invalid line of parameter: " + line);
        mName = s[0];
        String [] vs = s[1].split(",");
        if (vs.length == 0)
            throw new ParameterDefinitionException("empty values of parameter: " + line);
        mValues = new ArrayList<Double>();
        for (int i=0;i<vs.length;i++) {
            mValues.add(Double.parseDouble(vs[i]));
        }
    }

    public String getName() {
        return mName;
    }

    public Double[] getValues() {
        return mValues.toArray(new Double[mValues.size()]);
    }

    public int getNumberOfValues() {
        return mValues.size();
    }

    public Double getValue(int i) {
        return mValues.get(i);
    }
}
