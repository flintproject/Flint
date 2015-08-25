/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp;

import jp.oist.flint.phsp.entity.Model;

public interface IPhspConfiguration {

    public Model[] getModels() throws PhspException;

}
