/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.k3;

import org.apache.log4j.Logger;
import org.dom4j.DocumentException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;

public class K3Client {

    public int submit(final K3Request request)
        throws DocumentException,
               K3Exception,
               KeyManagementException,
               IOException,
               MalformedURLException,
               NoSuchAlgorithmException,
               NumberFormatException {
        K3Rest rest = new K3Rest(request.getAuthModel());
        String sedml = request.getSedml();
        Logger.getRootLogger().debug(sedml);
        K3JobModel job = rest.getJobSubmit(request.getJobTitle(),
                                           "", // TODO
                                           request.getModel(),
                                           sedml);
        String jobId = job.getJobId();
        return Integer.parseInt(jobId);
    }
}
