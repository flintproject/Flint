/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import java.util.concurrent.Callable;

public interface IJob<E> extends Callable<E> {

    public String getCommand();

    public Process getProcess();

}
