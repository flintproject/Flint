/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import org.apache.log4j.Logger;

public class Rescue extends Thread {

    private final Session mSession;

    public Rescue(final Session session) {
        this.mSession = session;
    }

    @Override
    public void run() {
        try {
            mSession.saveRecentModels();
        } catch (Exception e) {
            Logger.getRootLogger().error(e.getMessage());
        }
    }
}
