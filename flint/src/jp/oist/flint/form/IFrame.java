/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import com.google.protobuf.ByteString;

public interface IFrame {

    public void appendLog(String s);

    public void showErrorDialog(String message, String title);

    public void showErrorDialog(ByteString message, String title);

}
