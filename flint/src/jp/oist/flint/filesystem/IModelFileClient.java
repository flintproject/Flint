/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.filesystem;

public interface IModelFileClient {

    // This method is called from ModelFileWatcher
	// when the client's file is found modified.
    public void notifyModelFileModified();

}
