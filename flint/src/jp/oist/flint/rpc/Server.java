/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.rpc;

import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.BindException;
import java.net.InetAddress;
import java.net.InetSocketAddress;

public class Server {

    static final int PORT = 20465;

    private final HttpServer mHttpServer;

    public Server(ICallee callee) throws BindException, IOException {
        InetSocketAddress isa = new InetSocketAddress(InetAddress.getLoopbackAddress(), PORT);
        mHttpServer = HttpServer.create(isa, 0); // use the default value for the socket backlog
        mHttpServer.createContext("/open-model", new OpenModelRequestHandler(callee));
        mHttpServer.createContext("/run", new RunRequestHandler(callee));
    }

    public void start() {
        mHttpServer.setExecutor(null); // create a default executor
        mHttpServer.start();

        Runtime.getRuntime().addShutdownHook(new Thread() {
             @Override
             public void run() {
                 mHttpServer.stop(0);
             }
        });
    }
}
