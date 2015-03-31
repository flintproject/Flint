/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.garuda;

import jp.sbi.garuda.platform.commons.Gadget;
import java.util.List;

public interface ICompatibleGadgetClient {

    void loadGadgetsForCsv(List<Gadget> list);
    void loadGadgetsForIsd(List<Gadget> list);

}
