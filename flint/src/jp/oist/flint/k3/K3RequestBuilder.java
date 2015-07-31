/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.k3;

import jp.oist.flint.sedml.ISimulationConfiguration;
import jp.oist.flint.sedml.SedmlWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class K3RequestBuilder {

    final private File mModel;

    final private ISimulationConfiguration mSimulationConfiguration;

    public K3RequestBuilder(File model, ISimulationConfiguration simulationConfiguration) {
        mModel = model;
        mSimulationConfiguration = simulationConfiguration;
    }

    private String getSedml() throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        SedmlWriter writer = new SedmlWriter(false);
        writer.writeSimulationConfiguration(mSimulationConfiguration, baos);
        return new String(baos.toByteArray(), StandardCharsets.UTF_8);
    }

    private byte[] readModel() throws IOException {
        int len = (int)mModel.length();
        byte[] buf = new byte[len];
        try (FileInputStream inputStream = new FileInputStream(mModel)) {
        inputStream.read(buf, 0, len);
        }
        return buf;
    }

    public K3Request build(String title, String username, char[] password) throws IOException {
        return build(title, username, new String(password));
    }

    public K3Request build(String title, String username, String password) throws IOException {
        K3Request request = new K3Request(username, password);
        request.setJobTitle(title);
        request.setModel(new String(readModel(), StandardCharsets.UTF_8));
        request.setSedml(getSedml());
        return request;
    }
}
