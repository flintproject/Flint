/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.k3;

import org.apache.log4j.Logger;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;

/**
 * RESTアクセス関連クラス
 *
 * RESTアクセスに関連するメソッドを提供します。
 */
public class K3Rest {

    /** REST URL:本系ドメイン */
    private static final String URL_TOP_H = "https://flintk3.unit.oist.jp";

    /** REST URL:ジョブリスト(部品) */
    private static final String URL_JOB_LIST_PT = "/authifs/@UID@/job/list";

    /** REST URL:ジョブ詳細(部品) */
    private static final String URL_JOB_DETAIL_PT = "/authifs/@UID@/job/@JOBID@";

    /** REST URL:ジョブ結果グラフ(部品) */
    private static final String URL_JOB_GRAPH_PT = "/authifs/@UID@/job/@JOBID@/graph";

    /** REST URL:ジョブ結果CSV(部品) */
    private static final String URL_JOB_CSV_PT = "/authifs/@UID@/job/@JOBID@/result";

    /** REST URL:ジョブサブミット(部品) */
    private static final String URL_JOB_SUBMIT_PT = "/authifs/@UID@/job";

    /** REST URL:ジョブリスト */
    private static String URL_JOB_LIST = "";

    /** REST URL:ジョブ詳細 */
    private static String URL_JOB_DETAIL = "";

    /** REST URL:ジョブ結果グラフ */
    private static String URL_JOB_GRAPH = "";

    /** REST URL:ジョブ結果CSV */
    private static String URL_JOB_CSV = "";

    /** REST URL:ジョブサブミット */
    private static String URL_JOB_SUBMIT = "";

    /** 認証PASSWORD */
    private static String ups;

    /**
     * コンストラクタ
     *
     * <pre>
     * 認証モデルを引数としてモデル内より認証情報、デバッグモード、ジョブIDを取得する。
     * 取得した情報を元に各種REST APIのURLを生成する。
     *
     * ※ デバッグモードの判断で接続先をローカル環境(local.oist.jp)と本系環境の
     * 　 切り分けを行う。ユーザIDやジョブIDはURIに埋め込み生成する。
     * </pre>
     *
     * @param authModel 認証モデル
     */
    public K3Rest(K3AuthModel authModel) {

        // @UID@の置換、@JOBID@の置換
        URL_JOB_LIST   = URL_JOB_LIST_PT.replaceAll("@UID@", authModel.getUid());
        URL_JOB_DETAIL = URL_JOB_DETAIL_PT.replaceAll("@UID@", authModel.getUid()).replaceAll("@JOBID@", authModel.getJobId());
        URL_JOB_GRAPH  = URL_JOB_GRAPH_PT.replaceAll("@UID@", authModel.getUid()).replaceAll("@JOBID@", authModel.getJobId());
        URL_JOB_CSV    = URL_JOB_CSV_PT.replaceAll("@UID@", authModel.getUid()).replaceAll("@JOBID@", authModel.getJobId());
        URL_JOB_SUBMIT = URL_JOB_SUBMIT_PT.replaceAll("@UID@", authModel.getUid());

        // 本系の時は実サイト接続Uri生成
        URL_JOB_LIST   = URL_TOP_H + URL_JOB_LIST;
        URL_JOB_DETAIL = URL_TOP_H + URL_JOB_DETAIL;
        URL_JOB_GRAPH  = URL_TOP_H + URL_JOB_GRAPH;
        URL_JOB_CSV    = URL_TOP_H + URL_JOB_CSV;
        URL_JOB_SUBMIT = URL_TOP_H + URL_JOB_SUBMIT;

        // パスワード
        ups = authModel.getUps();
    }

    /**
     * ジョブをサブミットする.
     *
     * <pre>
     * 新規ジョブ登録のREST APIを利用し、
     * 引数にPOSTし新たにジョブをリクエストします。
     * 実行結果はジョブモデルに格納し戻します。
     * </pre>
     *
     * @param jobname ジョブ名
     * @param email メールアドレス
     * @param model モデル XML文字列
     * @param sedml SED-ML XML文字列
     * @return jobModel ジョブモデルに結果値を格納し戻します
     */
    public K3JobModel getJobSubmit(String jobname, String email,
                                 String model, String sedml)
        throws ConnectException,
               DocumentException,
               K3Exception,
               KeyManagementException,
               IOException,
               MalformedURLException,
               NoSuchAlgorithmException {

        // 戻り値のインスタンスを初期化します。
        K3JobModel jobModel = new K3JobModel();

        // サブミット用URL
        Logger.getRootLogger().debug("## getJobSubmit URL:" + URL_JOB_SUBMIT);

        // POSTパラメータ格納用
        Map<String, String> cond = new HashMap<>();
        // パスワード
        cond.put("loginps", ups);
        // ジョブ名
        cond.put("jobname", jobname);
        // Email 任意
        cond.put("email", email);
        // モデル
        cond.put("model", model);
        // SED-ML
        cond.put("sedml", sedml);

        // RESTアクセスを呼びます
        String resultXML = restPost(URL_JOB_SUBMIT, cond);
        // 取得のXMLをDocオブジェクトに格納
        Document doc = DocumentHelper.parseText(resultXML);
        // 読み込みXMLをデバッグ表示
        Logger.getRootLogger().debug(doc.asXML());
        // XML のタイトルを取得し判定する
        String xmlTitle = doc.getRootElement().elementText("title");

        if (xmlTitle instanceof String && xmlTitle.equals("error")) {
            throw new K3Exception(doc.getRootElement().element("entry").elementText("content"));
        } else {
            // モデルに設定する
            jobModel = modelFromXML(doc);
        }

        // モデルを戻します。
        return jobModel;
    }

    /**
     * RESTサイトより取得した情報をテキスト文字列として戻します。
     *
     * <pre>
     * REST URLに対して引数のPOSTパラメータと共にリクエストに乗せて送信します。
     * URLに関してはRESTアクセスのパターンにしたがって以下の通りとする。
     * =========================================================================
     * ★ジョブの一覧を取得する(ログイン時の認証を含む) ---------------------------
     *   URL "https://flint.unit.oist.jp/authifs/{ユーザID}/job/list"
     * 　この時必要なPOSTパラメータ
     *      postParam("loginps", "パスワード値")
     *
     * =========================================================================
     * ★ジョブの詳細を取得する --------------------------------------------------
     *   URL "https://flint.unit.oist.jp/authifs/{ユーザID}/job/{ジョブID}"
     * 　この時必要なPOSTパラメータ
     *      postParam("loginps", "パスワード値")
     *
     * =========================================================================
     * ★新たなジョブをサブミットする --------------------------------------------
     *   URL "https://flint.unit.oist.jp/authifs/{ユーザID}/job"
     * 　この時必要なPOSTパラメータ
     *      postParam("loginps", "パスワード値")
     *      postParam("jobname", "ジョブ名値")
     *      postParam("email",   "Email値") *任意
     *      postParam("model",   "モデル値")
     *      postParam("sedml",   "SED-ML値")
     * =========================================================================
     * </pre>
     *
     * @param uri RESTアクセスURL
     * @param postParam POSTするパラメータ群MAP
     * @return String XML形式の改行付きテキスト情報
     *
     * @throws MalformedURLException URL不正関連
     * @throws UnsupportedEncodingException エンコード関連
     * @throws IOException ファイルIO関連
     * @throws ConnectException 接続できない場合
     */
    private static String restPost(String uri, Map<String, String> postParam)
        throws KeyManagementException,
               MalformedURLException,
               NoSuchAlgorithmException,
               IOException,
               UnsupportedEncodingException,
               ConnectException {

        // コネクションを取得する
        HttpURLConnection http = httpConnect(uri, postParam);

        // Responceを受信します。
        StringBuilder resString = new StringBuilder();
        BufferedReader reader = new BufferedReader(
                            new InputStreamReader(http.getInputStream()));
        String line;
        while((line = reader.readLine()) != null) {
            resString.append(line);
            resString.append("\n");
        }

        // デバッグ的にPOST後のレスポンス情報を出力します
        Logger.getRootLogger().debug("## Response Value Start --------------------------------");
        Logger.getRootLogger().debug(resString.toString());
        Logger.getRootLogger().debug("## Response Value End ----------------------------------");

        // デバッグ表示
        Logger.getRootLogger().debug("## DisConnect");
        // 切断
        http.disconnect();

        // 戻します
        return resString.toString();
    }

    /**
     * URLに接続しコネクションを戻します。
     *
     * <pre>
     * 引数のURLを元に接続、POSTパラメータの送信までを実施しコネクションを戻します。
     * コネクションやリターンコードが200以外の場合を含め接続異常時
     * はエクセプションを戻します。
     *
     * ※ デバッグモードの場合、SSL(https://)やSSLドメインが不正でも
     * 　 それを無視して接続できるようにします。
     * </pre>
     *
     * @param uri REST API接続のURI
     * @param postParam POSTしたいパラメータ(マップ形式にて複数)
     * @return httpコネクション
     * @throws MalformedURLException URL不正関連
     * @throws UnsupportedEncodingException エンコード関連
     * @throws IOException ファイルIO関連
     * @throws ConnectException 接続できない場合
     */
    private static HttpURLConnection httpConnect(String uri, Map<String, String> postParam)
        throws KeyManagementException,
               MalformedURLException,
               NoSuchAlgorithmException,
               UnsupportedEncodingException,
               IOException,
               ConnectException {

        // 接続先URL
        URL url = new URL(uri);
        // デバッグ表示
        Logger.getRootLogger().debug("## URL :" + uri);

        // 戻り値初期化
        HttpURLConnection http;

        // POSTパラメータを生成します。
        // デバッグ表示
        Logger.getRootLogger().debug("## POST PARAMS EDIT -- Start ----------------------------");
        StringBuilder postString = new StringBuilder();
        if (postParam != null && !postParam.isEmpty()) {
            int postParamCount = 0;
            // 引数のPOSTパラメータ分繰り返します
            for (Map.Entry<String, String> postVal : postParam.entrySet()) {
                if (postParamCount > 0) {
                    postString.append("&");
                }
                // POSTのキー値
                postString.append(postVal.getKey());
                postString.append("=");
                // POSTの値(UTF8にエンコードして設定します)
                postString.append(URLEncoder.encode(postVal.getValue(), "utf-8"));
                postParamCount++;
                // デバッグ表示
                Logger.getRootLogger().debug(postParamCount + ".POST-PARAM:" +  postVal.getKey() + "=" + URLEncoder.encode(postVal.getValue(), "utf-8"));
            }

        } else {

            // デバッグ表示
            Logger.getRootLogger().debug("*** BUG POST Params is NULL ***");
            // パラメータが無い場合は空で戻します
        }

        // デバッグ表示
        Logger.getRootLogger().debug("## POST PARAMS EDIT -- End ------------------------------");

        // コネクション
        http = (HttpURLConnection) url.openConnection();
        http.setDoOutput(true);
        http.setDoInput(true);
        http.setRequestMethod("POST");
        http.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
        http.setRequestProperty("Content-Length", String.valueOf(postString.toString().getBytes().length));

        // デバッグモードがtrueの場合のみこの処理は実行します。
        // SSLが似非、ホストが似非の場合でも通す仕組みを通します。
        //nonCheckSSL(http);

        // リダイレクトまでは追わないものとします。(そんなサイトへの接続はしないが)
        http.setInstanceFollowRedirects(false);
        HttpURLConnection.setFollowRedirects(false);

        // デバッグ表示
        Logger.getRootLogger().debug("## Connect");
        // 接続します。（接続不可でConnectExceptionを戻します）
        http.connect();

        // POST送信をします。
        try (OutputStreamWriter osw = new OutputStreamWriter(http.getOutputStream())) {
        osw.write(postString.toString());
        }

        // レスポンスコードを確認します
        int res = http.getResponseCode();
        if (res != 200) {

            // デバッグ表示
            Logger.getRootLogger().debug("*** HTTP RESPONSE CODE ( "+ res +" ) IS BAT ***");
            // スローします
            throw new ConnectException("RESPONSE CODE ( "+ res +" )");
        }
        // デバッグ表示
        Logger.getRootLogger().debug("## HTTP ResponseCode:" + res);

        // HttpURLConnection を戻します
        return http;
    }

    /**
     * 似非SSLや似非SSL-HOSTを通すための処理を定義します。
     *
     * <pre>
     * 引数のコネクションに対してSSL関係の不備を無視させて通します。
     * ※ デバッグモードの場合のみ実行される
     * （正規SSLであればこの処理を通さずして問題なく通るのでデバッグ時のみ。）
     * </pre>
     *
     * @param http httpコネクション
     */
    private static void nonCheckSSL(HttpURLConnection http)
        throws KeyManagementException, NoSuchAlgorithmException {
        // SSL接続で証明書の正当性を無視させるために何もしないTrustManagerを生成しときます
        TrustManager nonCheckTrustManager = new X509TrustManager() {
                    @Override
                public void checkClientTrusted(X509Certificate[] chain,
                                               String authType) throws CertificateException {
                }
                    @Override
                public void checkServerTrusted(X509Certificate[] chain,
                                               String authType) throws CertificateException {
                }
                    @Override
                public X509Certificate[] getAcceptedIssuers() {
                    return null;
                }
            };

        //
        SSLContext sslcontext = SSLContext.getInstance("TLS");
        sslcontext.init(null, new TrustManager[] { nonCheckTrustManager }, null);
        ((HttpsURLConnection) http).setSSLSocketFactory(sslcontext.getSocketFactory());

        // 証明書にあるホスト名とアクセスしているホスト名の違いを無視する
        ((HttpsURLConnection) http).setHostnameVerifier(
                                                        new javax.net.ssl.HostnameVerifier() {
                                                                                                                    @Override
                                                            public boolean verify(String host, javax.net.ssl.SSLSession ses) {
                                                                return true;
                                                            }
                                                        }
                                                        );
    }

    /**
     * XMLを解析し、ジョブモデルに値を格納して返す.
     *
     * <pre>
     * 引数のドキュメントオブジェクトよりREST APIで戻されるXML形式の情報を解析し
     * ジョブモデルに格納、Entryタグ単位ごとにリストオブジェクトへ追加した結果の
     * １件目のみをジョブモデルとして戻します。
     * </pre>
     *
     * @param doc XMLドキュメント
     * @return jobModel ジョブモデル
     */
    private static K3JobModel modelFromXML(Document doc) {

        // ジョブモデルリストを取得しモデルを返す
        List<K3JobModel> jobModelList = modelListFromXML(doc);

        return jobModelList.get(0);

    }

    /**
     * XMLを解析し、ジョブモデルに値を格納し、リストにして返す.
     *
     * <pre>
     * 引数のドキュメントオブジェクトよりREST APIで戻されるXML形式の情報を解析し
     * ジョブモデルに格納、Entryタグ単位ごとにリストオブジェクトへ追加し戻します。
     * </pre>
     *
     * @param doc XMLドキュメント
     * @return jobModelList ジョブモデルリスト
     */
    private static List<K3JobModel> modelListFromXML(Document doc) {

        // 戻り値
        List<K3JobModel> jobModelList = new ArrayList<>();

            // ジョブモデルのインスタンス生成
            K3JobModel jobModel;
            // rootから親要素(entry)を取得する
            List<Element> parentElements = doc.getRootElement().elements("entry");
            for (Element parentElement : parentElements) {
                jobModel = new K3JobModel();
                jobModel.setJobTitle(parentElement.elementText("title"));
                jobModel.setJobId(parentElement.elementText("id"));
                jobModel.setJobLastUpDate(parentElement.elementText("updated"));
                jobModel.setIssued(parentElement.elementText("issued"));
                jobModel.setJobInformation(parentElement.elementText("content"));
                // 認証情報を取り出す
                List<Element> authElements = parentElement.elements("author");
                for (Element authElement : authElements) {
                    jobModel.setAuthorName(authElement.elementText("name"));
                    jobModel.setAuthorEmail(authElement.elementText("email"));
                }
                jobModelList.add(jobModel);
            }
        return jobModelList;
    }
}
