/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.component;

import java.io.File;
import java.util.Map;

public class Linux implements IOs {

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
        File appendFile = command.getAppendFile();
        if (appendFile != null) {
            s += " >> " + getQuotedFilePath(appendFile);
        }
        return s;
    }

    @Override
    public String getQuotedFilePath(File file) {
        return "'" + file.getPath() + "'";
    }

    @Override
    public void setUpEnvironment(ProcessBuilder pb, File hint) {
        // There is nothing to do if hint is null
        if (hint == null) return;
        Map<String, String> env = pb.environment();
        String origPath = env.get("PATH");
        String path = hint.getAbsolutePath();
        if (origPath == null) {
            env.put("PATH", path);
        } else {
            env.put("PATH", path + File.pathSeparator + origPath);
        }
    }
}
