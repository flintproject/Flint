/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.rpc;

import jp.oist.flint.headless.Oneshot;
import jp.physiome.Cli.RunOption;
import com.google.protobuf.InvalidProtocolBufferException;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.IOException;
import javax.swing.SwingUtilities;

public class RunRequestHandler extends RequestHandler implements HttpHandler {

    public RunRequestHandler(ICallee callee) {
        super(callee);
    }

    @Override
    public void handle(HttpExchange exchange) throws IOException {
        try {
            validatePost(exchange);

            Headers requestHeaders = exchange.getRequestHeaders();
            byte[] body = readRequestBody(exchange, requestHeaders);
            RunOption option = RunOption.parseFrom(body);

            respondOk(exchange);

            Oneshot oneshot = new Oneshot(option, false);
            SwingUtilities.invokeLater(oneshot);
        } catch (InvalidProtocolBufferException ipbe) {
            respondToBadRequest(exchange, ipbe);
        } catch (RpcException re) {
            respondToBadRequest(exchange, re);
        }
    }
}
