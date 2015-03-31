/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.rpc;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

public class RequestHandler {

    static final int LENGTH = 2048;

    protected final ICallee mCallee;

    protected RequestHandler(ICallee callee) {
        mCallee = callee;
    }

    protected void validatePost(HttpExchange exchange) throws RpcException {
        String method = exchange.getRequestMethod();
        if (!"POST".equals(method)) {
            throw new RpcException("POST method expected, but got " + method);
        }
    }

    protected byte[] readRequestBody(HttpExchange exchange, Headers headers)
        throws IOException, RpcException {
        String contentLength = headers.getFirst("Content-Length");
        int length;
        if (contentLength instanceof String) {
            try {
                length = Integer.parseInt(contentLength);
            } catch (NumberFormatException nfe) {
                throw new RpcException("invalid value of Content-Length: " + contentLength, nfe);
            }
            if (length <= 0) {
                throw new RpcException("invalid value of Content-Length: " + contentLength);
            }
        } else {
            length = LENGTH;
        }
        byte[] buf = new byte[length];
        int len = 0;
        try (InputStream is = exchange.getRequestBody()) {
            while (len < length) {
                int s = is.read(buf);
                if (s < 0) break;
                len += s;
            }
        }
        if (contentLength instanceof String) {
            if (length != len) {
                throw new RpcException("failed to read request body: " + length + " vs " + len);
            }
        }
        return Arrays.copyOf(buf, length);
    }

    protected void respondOk(HttpExchange exchange) throws IOException {
        Headers responseHeaders = exchange.getResponseHeaders();
        exchange.sendResponseHeaders(200, 0);
        try (OutputStream os = exchange.getResponseBody()) {
            // TODO
        }
    }

    protected void respondToBadRequest(HttpExchange exchange, Throwable cause) throws IOException {
        exchange.sendResponseHeaders(400, 0);
        try (OutputStream os = exchange.getResponseBody();
             DataOutputStream dos = new DataOutputStream(os)) {
            dos.writeBytes(cause.getMessage());
        }
    }

}
