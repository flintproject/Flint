/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.component;

import java.io.File;

public class Windows implements IOs {

    @Override
    public String getCommandString(Command command, File path) {
        StringBuilder sb = new StringBuilder(command.getName());
        for (String option : command.getOptions()) {
            sb.append(' ');
            sb.append(option);
        }
        for (Object argument : command.getArguments()) {
            sb.append(' ');
            if (argument instanceof File) {
                sb.append(getQuotedFilePath((File)argument));
            } else {
                sb.append(argument);
            }
        }
        File inputFile = command.getInputFile();
        if (inputFile != null) {
            sb.append(" < ");
            sb.append(getQuotedFilePath(inputFile));
        }
        File outputFile = command.getOutputFile();
        if (outputFile != null) {
            sb.append(" > ");
            sb.append(getQuotedFilePath(outputFile));
        }
        return sb.toString();
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
