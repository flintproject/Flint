/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.k3;

/**
 * 実行ジョブの情報を格納するモデルクラス
 */
public class K3JobModel {

    /** JOBリクエストタイトル */
    private String jobTitle;

    /** JOBリクエスID */
    private String jobId;

    /** JOB最終更新日 */
    private String jobLastUpDate;

    /** JOB作成者名 */
    private String authorName;

    /** JOB作成者EMAIL */
    private String authorEmail;

    /** JOBリクエスト日時 */
    private String issued;

    /** JOBの指示状態 */
    private String jobInformation;

    /**
     * ジョブタイトルを戻します
     *
     * @return the jobTitle
     */
    public String getJobTitle() {
        return jobTitle;
    }

    /**
     * ジョブタイトルを設定します
     *
     * @param jobTitle the jobTitle to set
     */
    public void setJobTitle(String jobTitle) {
        this.jobTitle = jobTitle;
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

    /**
     * ジョブの最終更新日を戻します
     *
     * @return the jobLastUpDate
     */
    public String getJobLastUpDate() {
        return jobLastUpDate;
    }

    /**
     * ジョブの最終更新日を設定します
     *
     * @param jobLastUpDate the jobLastUpDate to set
     */
    public void setJobLastUpDate(String jobLastUpDate) {
        this.jobLastUpDate = jobLastUpDate;
    }

    /**
     * 作成者名を戻します
     *
     * @return the authorName
     */
    public String getAuthorName() {
        return authorName;
    }

    /**
     * 作成者名を設定します
     *
     * @param authorName the authorName to set
     */
    public void setAuthorName(String authorName) {
        this.authorName = authorName;
    }

    /**
     * メールアドレスを戻します
     *
     * @return the authorEmail
     */
    public String getAuthorEmail() {
        return authorEmail;
    }

    /**
     * メールアドレスを設定します
     *
     * @param authorEmail the authorEmail to set
     */
    public void setAuthorEmail(String authorEmail) {
        this.authorEmail = authorEmail;
    }

    /**
     * ジョブリクエスト日時を戻します
     *
     * @return the issued
     */
    public String getIssued() {
        return issued;
    }

    /**
     * ジョブリクエスト日時を設定します
     *
     * @param issued the issued to set
     */
    public void setIssued(String issued) {
        this.issued = issued;
    }

    /**
     * ジョブステータス情報を戻します
     *
     * @return the jobInformation
     */
    public String getJobInformation() {
        return jobInformation;
    }

    /**
     * ジョブステータス情報を設定します
     *
     * @param jobInformation the jobInformation to set
     */
    public void setJobInformation(String jobInformation) {
        this.jobInformation = jobInformation;
    }
}
