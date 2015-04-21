/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.component;

import java.io.File;

public class Windows implements IOs {

    @Override
    public String getCommandString(Command command, File path) {
        String s = command.getName();
        for (String option : command.getOptions()) {
            s += " " + option;
        }
        for (Object argument : command.getArguments()) {
            if (argument instanceof File) {
                s += " " + getQuotedFilePath((File)argument);
            } else {
                s += " " + argument.toString();
            }
        }
        File inputFile = command.getInputFile();
        if (inputFile != null) {
            s += " < " + getQuotedFilePath(inputFile);
        }
        File outputFile = command.getOutputFile();
        if (outputFile != null) {
            s += " > " + getQuotedFilePath(outputFile);
        }
        return s;
    }

    @Override
    public String getQuotedFilePath(File file) {
        return "\"" + file.getPath() + "\"";
    }

    @Override
    public void setUpEnvironment(ProcessBuilder pb, File hint) {
        // nothing to do
    }
}
