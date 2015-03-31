/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.component;

import java.io.File;

public interface IOs {

    public String getCommandString(Command command, File path);

    public String getQuotedFilePath(File file);

    public void setUpEnvironment(ProcessBuilder pb, File hint);

}
