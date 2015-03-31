/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.component;

import java.util.ArrayList;

public class CommandLine {

    final ArrayList<Command> mCommands = new ArrayList<>();

    public CommandLine(Command command) {
        mCommands.add(command);
    }

    public ArrayList<Command> getCommands() {
        return mCommands;
    }

}
