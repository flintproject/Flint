/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.rpc;

public interface ICallee {

    /*
     * This method must be safe even if the caller thread is not Swing's EDT.
     */
    public void openModel(final String name);

}
