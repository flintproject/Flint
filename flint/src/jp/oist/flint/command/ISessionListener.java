/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import java.io.File;
import java.util.ArrayList;
import java.util.EventListener;

public interface ISessionListener extends EventListener {

    /**
     * This method is called from EDT immediately after the list of recent models has been updated.
     * @param recentModels the updated list
     */
    void recentModelsUpdated(final ArrayList<File> recentModels);

}
