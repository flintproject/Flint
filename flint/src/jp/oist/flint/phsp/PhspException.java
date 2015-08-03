/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp;

public class PhspException extends Exception {

    public PhspException(String message) {
        super(message);
    }

    public PhspException(String message, Throwable cause) {
        super(message, cause);
    }
}
