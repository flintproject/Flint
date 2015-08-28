/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import java.util.EventListener;

public interface ILoadingListener extends EventListener {

    /**
     * This method may or may not be called once loading is started.
     */
    void loadingStarted();

    /**
     * This method is called once loading is done.
     */
    void loadingDone();

}
