/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import jp.physiome.Ipc;

public class IntegrationMethodFormat {

    public static String name(Ipc.IntegrationMethod method) {
        if (method == null) return null;
        switch (method) {
        case EULER:         return "Euler";
        case RUNGE_KUTTA:   return "Runge-Kutta";
        case ARK: return "ARK";
        }
        return null;
    }

    public static String kisaoId(Ipc.IntegrationMethod method) {
        if (method == null) return null;
        switch (method) {
        case EULER:         return "0000030";
        case RUNGE_KUTTA:   return "0000032";
        case ARK: return "9999999"; // TODO: use a proper ID if it appears
        }
        return null;
    }

}
