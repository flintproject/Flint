/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.k3;

/**
 * 認証情報を格納するモデルクラス
 */
public class K3AuthModel {

    /** ログインID */
    private String uid;

    /** ログインパスワード */
    private String ups;

    /** JOB ID */
    private String jobId;

    public K3AuthModel() {
        jobId = ""; // FIXME
    }

    /**
     * ログインIDを戻します
     *
     * @return the uid
     */
    public String getUid() {
        return uid;
    }

    /**
     * ログインIDを設定します
     *
     * @param uid the uid to set
     */
    public void setUid(String uid) {
        this.uid = uid;
    }

    /**
     * ログインパスワードを戻します
     *
     * @return the ups
     */
    public String getUps() {
        return ups;
    }

    /**
     * ログインパスワードを設定します
     *
     * @param ups the ups to set
     */
    public void setUps(String ups) {
        this.ups = ups;
    }

    /**
     * ジョブIDを戻します
     *
     * @return the jobId
     */
    public String getJobId() {
        return jobId;
    }

    /**
     * ジョブIDを設定します
     *
     * @param jobId the jobId to set
     */
    public void setJobId(String jobId) {
        this.jobId = jobId;
    }
}
