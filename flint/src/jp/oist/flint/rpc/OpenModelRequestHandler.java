/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.rpc;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.IOException;

public class OpenModelRequestHandler extends RequestHandler implements HttpHandler {

    public OpenModelRequestHandler(ICallee callee) {
        super(callee);
    }

    @Override
    public void handle(HttpExchange exchange) throws IOException {
        try {
            validatePost(exchange);

            Headers requestHeaders = exchange.getRequestHeaders();

            byte[] body = readRequestBody(exchange, requestHeaders);
            String name = new String(body, "UTF-8");

            respondOk(exchange);

            mCallee.openModel(name);
        } catch (RpcException re) {
            respondToBadRequest(exchange, re);
        }
    }
}
