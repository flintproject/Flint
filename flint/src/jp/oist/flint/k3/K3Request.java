/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.k3;

public class K3Request {

    private final K3AuthModel mAuthModel = new K3AuthModel();

    private String mJobTitle;
    private String mModel;
    private String mSedml;

    public K3Request(String uid, String password) {
        mAuthModel.setUid(uid);
        mAuthModel.setUps(password);
    }

    public K3AuthModel getAuthModel() {
        return mAuthModel;
    }

    public void setJobTitle(String title) {
        mJobTitle = title;
    }

    public String getJobTitle() {
        return mJobTitle;
    }

    public void setModel(String model) {
        mModel = model;
    }

    public String getModel() {
        return mModel;
    }

    public void setSedml(String sedml) {
        mSedml = sedml;
    }

    public String getSedml() {
        return mSedml;
    }
}
