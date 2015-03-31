/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

public class JobException extends Exception {

    public JobException(IJob job) {
        super(job.getClass().getName());
    }

    public JobException(String message, Throwable cause) {
        super(message, cause);
    }
}
