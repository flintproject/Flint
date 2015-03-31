/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.log4j.Logger;
import jp.physiome.Cli.RunOption;

public class CommandLineArguments {

    private Options mOptions = new Options();

    private CommandLineParser mParser = new GnuParser();

    private CommandLine mCommandLine;

    private boolean mIsHeadless = false;
    private RunOption.Builder mHeadlessBuilder = RunOption.newBuilder();

    public CommandLineArguments() {
        Option headless = OptionBuilder.withArgName("input> <output")
            .hasArgs(2)
            .withDescription("turn on the headless mode")
            .create("headless");
        Option help = new Option("h", "help", false, "print this message");
        Option e = OptionBuilder.withArgName("file")
            .hasArg()
            .withDescription("error log for the headless mode")
            .create("e");
        Option g = OptionBuilder.withArgName("n")
            .hasArg()
            .withType(Number.class)
            .withDescription("sampling rate for the headless mode")
            .create("g");
        Option s = OptionBuilder.withArgName("file")
            .hasArg()
            .withDescription("output variables for the headless mode")
            .create("s");

        mOptions.addOption(headless);
        mOptions.addOption(help);
        mOptions.addOption(e);
        mOptions.addOption(g);
        mOptions.addOption(s);
    }

    private void printHelp() {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("Flint", mOptions);
    }

    public void parse(String[] args) {
        try {
            mCommandLine = mParser.parse(mOptions, args);
        } catch (ParseException pe) {
            Logger.getRootLogger().fatal("could not parse the command line: " + pe.getMessage());
            System.exit(1);
        }

        // if help requested, print message and exit normally
        if (mCommandLine.hasOption("help")) {
            printHelp();
            System.exit(0);
        }

        // the headless option should have input/output arguments
        if (mCommandLine.hasOption("headless")) {
            String[] values = mCommandLine.getOptionValues("headless");
            if (values.length < 2) {
                printHelp();
                System.exit(1);
            }
            mIsHeadless = true;
            mHeadlessBuilder.setModelFilename(values[0]);
            mHeadlessBuilder.setOutputFilename(values[1]);
            if (mCommandLine.hasOption("e")) {
                mHeadlessBuilder.setErrorFilename(mCommandLine.getOptionValue("e"));
            }
            if (mCommandLine.hasOption("g")) {
                try {
                    int n = ((Number)mCommandLine.getParsedOptionValue("g")).intValue();
                    if (n <= 0) {
                        Logger.getRootLogger().fatal("-g expects a positive integer, but " + mCommandLine.getOptionValue("g"));
                        System.exit(1);
                    }
                    mHeadlessBuilder.setGranularity(n);
                } catch (ParseException pe) {
                    Logger.getRootLogger().fatal(pe.getMessage());
                    System.exit(1);
                }
            }
            if (mCommandLine.hasOption("s")) {
                mHeadlessBuilder.setSpecFilename(mCommandLine.getOptionValue("s"));
            }
        }
    }

    public boolean isHeadless() {
        return mIsHeadless;
    }

    public RunOption getHeadlessOption() {
        return mHeadlessBuilder.build();
    }

    public String[] getPaths() {
        return mCommandLine.getArgs();
    }
}
