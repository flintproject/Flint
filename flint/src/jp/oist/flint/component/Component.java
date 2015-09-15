/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.component;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Component {

    enum Shell {
        CMD,
        SH
    }

    private static final Shell mShell;

    static {
        String osName = System.getProperty("os.name");
        if (osName != null && osName.startsWith("Windows")) {
            mShell = Shell.CMD;
        } else {
            mShell = Shell.SH;
        }
    }

    static private IOs mOs;

    static private File mPath = null;

    public static void setOs(IOs os) {
        mOs = os;
    }

    public static void setPath(File path) {
        mPath = path;
    }

    public static void setUpEnvironment(ProcessBuilder pb) {
        mOs.setUpEnvironment(pb, mPath);
    }

    public static ArrayList<String> getOpenCommand() {
        return getCommandByName("flint-open");
    }

    private static ArrayList<String> getCommandLineString(CommandLine commandLine) {
        ArrayList<String> list = new ArrayList<>();
        if (mShell == Shell.CMD) {
            list.add("cmd");
            list.add("/c");
        } else {
            list.add("/bin/sh");
            list.add("-c");
        }
        String s = null;
        for (Command command : commandLine.getCommands()) {
            String cs = mOs.getCommandString(command, mPath);
            if (s == null) {
                s = cs;
            } else {
                s += " | " + cs;
            }
        }
        list.add(s);
        return list;
    }

    public static List<String> getFlintExecCommand() {
        return getCommandByName("flint-exec");
    }

    public static List<String> getFlintRunCommand() {
        return getCommandByName("flint-run");
    }

    public static List<String> getFlintPauseCommand(int pid) {
        Command command = new Command("flint-pause");
        command.addArgument(pid);
        return getCommandLineString(new CommandLine(command));
    }

    public static List<String> getFlintResumeCommand(int pid) {
        Command command = new Command("flint-resume");
        command.addArgument(pid);
        return getCommandLineString(new CommandLine(command));
    }

    public static List<String> getIsd2csvCommand(File inputFile, File outputFile) {
        Command command = new Command("isd2csv");
        command.addArgument(inputFile);
        command.setOutputFile(outputFile);
        return getCommandLineString(new CommandLine(command));
    }

    public static List<String> getIsd2csvCommand(File inputFile, File outputFile, int port) {
        Command command = new Command("isd2csv");
        command.addOption("progress", port);
        command.addArgument(inputFile);
        command.setOutputFile(outputFile);
        return getCommandLineString(new CommandLine(command));
    }

    public static ArrayList<String> getCommandByName(String name) {
        ArrayList<String> list = new ArrayList<>();
        if (mShell == Shell.CMD) {
            list.add("cmd");
            list.add("/c");
        } else {
            list.add("/bin/sh");
            list.add("-c");
        }
        list.add(name);
        return list;
    }
}
