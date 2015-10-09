/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.executor.PhspSimulator;
import jp.oist.flint.form.sub.JobWindow;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.phsp.entity.ParameterSet;
import jp.oist.flint.phsp.entity.TargetSet;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.Arrays;
import javax.xml.parsers.ParserConfigurationException;

class ProgressDetailActionListener implements ActionListener {

    private final JobWindow mWindow;

    public ProgressDetailActionListener(PhspSimulator simulator, SubFrame subFrame)
        throws IOException, ParserConfigurationException {
        TargetSet ts = subFrame.getTargetSet();
        ParameterSet ps = subFrame.getParameterSet();
        ParameterSet newPs = ps.filterByNames(Arrays.asList(ts.getUsingParameterNames()));
        mWindow = new JobWindow(subFrame, String.format("Progress [%s]", subFrame.getModelCanonicalPath()));
        mWindow.load(newPs);
        mWindow.setSimulator(simulator);
        mWindow.setLocationRelativeTo(subFrame);
        simulator.addPropertyChangeListener(mWindow);
        subFrame.setJobWindow(mWindow);
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        mWindow.setVisible(true);
    }
}
