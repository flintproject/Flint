/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

public class SedmlException extends Exception {

    public SedmlException(String message) {
        super(message);
    }

    public SedmlException(String message, Throwable cause) {
        super(message, cause);
    }
}
