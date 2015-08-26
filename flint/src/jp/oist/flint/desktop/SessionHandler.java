/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.command.Session;

public class SessionHandler implements IDesktopListener {

    private final Session mSession;

    public SessionHandler(Session session) {
        mSession = session;
    }

    @Override
    public void documentAdded(Document doc) {
        mSession.updateRecentModels(doc.getFile());
    }

    @Override
    public void documentRemoved(Document doc, boolean empty) {
        // nothing to do
    }
}
