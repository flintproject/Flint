/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import java.util.EventListener;

public interface IDesktopListener extends EventListener {

    void documentAdded(Document doc);

    void documentRemoved(Document doc, boolean empty);

}
