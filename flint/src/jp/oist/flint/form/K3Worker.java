/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.k3.K3Client;
import jp.oist.flint.k3.K3Exception;
import jp.oist.flint.k3.K3Request;
import jp.oist.flint.k3.K3RequestBuilder;
import jp.oist.flint.sedml.ISimulationConfiguration;
import jp.oist.flint.sedml.SedmlException;
import org.dom4j.DocumentException;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.ExecutionException;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

class K3Worker extends SwingWorker<Integer, Void> {

    private final File mFile;
    private final ISimulationConfiguration mConfiguration;
    private final String mUserId;
    private final String mPassword;
    private final String mTitle;

    public K3Worker(File file, ISimulationConfiguration configuration, String userId, String password, String title) {
        mFile = file;
        mConfiguration = configuration;
        mUserId = userId;
        mPassword = password;
        mTitle = title;
    }

    @Override
    protected Integer doInBackground()
        throws DocumentException, IOException, K3Exception, KeyManagementException,
               MalformedURLException, NoSuchAlgorithmException, SedmlException {
        K3RequestBuilder reqBuilder = new K3RequestBuilder(mFile, mConfiguration);
        K3Request request = reqBuilder.build(mTitle, mUserId, mPassword);
        K3Client k3 = new K3Client();
        return k3.submit(request);
    }

    @Override
    protected void done() {
        try {
            int jobId = get();
            String message = String.format("Submitted successfully your job to Flint K3 (Job ID : %d)", jobId);
            String title = "Job submitted to Flint K3";
            JOptionPane.showMessageDialog(null,
                                          message,
                                          title,
                                          JOptionPane.INFORMATION_MESSAGE);
        } catch (InterruptedException | ExecutionException ex) {
            JOptionPane.showMessageDialog(null,
                                          ex.getMessage(),
                                          "Error on communicating with Flint K3",
                                          JOptionPane.ERROR_MESSAGE);
        }
    }
}
