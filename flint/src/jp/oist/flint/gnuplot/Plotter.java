/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.gnuplot;

import jp.oist.flint.control.FileChooser;
import jp.oist.flint.form.sub.IChartController;
import jp.oist.flint.form.sub.IChartSetting;
import jp.oist.flint.isdf.IsdfException;
import jp.oist.flint.isdf.IsdfHeader;
import jp.oist.flint.isdf.IsdfReader;
import jp.oist.flint.plot.gnuplot.GnuPlotter;
import jp.oist.flint.plot.gnuplot.Gnuplot;
import jp.oist.flint.util.ListItemModel;
import jp.oist.flint.util.Utility;
import jp.oist.flint.plotter.IPlotter;
import org.apache.log4j.Logger;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.GroupLayout;
import javax.swing.SwingWorker;

public class Plotter implements IPlotter {
    static final String DIRECTORY = "/jp/oist/flint/gnuplot";
    static final String NAME = "Gnuplot";

    static final int ISDF_HEADER_SIZE = 40;

    private static class GnuplotDialog extends JDialog {
        final JTextField mTf = new JTextField();
        final JLabel mLabel = new JLabel("executable path");
        final JButton mSelect = new JButton("Select");
        final JButton mOK = new JButton("OK");
        final JButton mCancel = new JButton("Cancel");

        public GnuplotDialog(final JDialog parent) {
            super(parent, NAME + " configuration", true);

            Preferences prefs = Preferences.userRoot().node(DIRECTORY);
            mTf.setText(prefs.get("path", null));

            mSelect.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent evt) {
                        FileChooser fc = new FileChooser(GnuplotDialog.this, "Select path", FileChooser.Mode.LOAD, mTf.getText());
                        if (fc.showDialog()) {
                            mTf.setText(fc.getSelectedFile().getAbsolutePath());
                        }
                    }
            });
            mOK.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent evt) {
                        Preferences prefs = Preferences.userRoot().node(DIRECTORY);
                        String path = mTf.getText();
                        if (path != null && !path.isEmpty()) {
                            prefs.put("path", path);
                            try {
                                prefs.sync();
                            } catch (BackingStoreException bse) {
                                Logger.getRootLogger().error(bse.getMessage());
                            }
                        }
                        dispose();
                    }
            });
            mCancel.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent evt) {
                        dispose();
                    }
            });

            GroupLayout layout = new GroupLayout(getContentPane());
            getContentPane().setLayout(layout);

            GroupLayout.SequentialGroup hGroup = layout.createSequentialGroup();
            hGroup.addGroup(layout.createParallelGroup().addComponent(mLabel));
            hGroup.addGroup(layout.createParallelGroup().addComponent(mTf).addGroup(layout.createSequentialGroup().addGap(10).addComponent(mOK).addGap(10).addComponent(mCancel).addGap(10)));
            hGroup.addGroup(layout.createParallelGroup().addComponent(mSelect));
            layout.setHorizontalGroup(hGroup);

            GroupLayout.SequentialGroup vGroup = layout.createSequentialGroup();
            vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).addComponent(mLabel).addComponent(mTf).addComponent(mSelect));
            vGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE).addComponent(mOK).addComponent(mCancel));
            layout.setVerticalGroup(vGroup);

            pack();
            setLocationRelativeTo(parent);
        }
    }

    public void plot(final IChartController controller) throws IOException, IsdfException, Exception {
        Preferences prefs = Preferences.userRoot().node(DIRECTORY);
        String path = prefs.get("path", null);
        if (path == null) {
            // TODO
        }

        String isdPath = controller.getIsdFile().getPath();
        String csvPath = controller.getCsvFile().getPath();
        String modelPath = controller.getModelFile().getCanonicalPath();

        IChartSetting setting =  controller.getChartSetting();
        GnuPlotter gp = new GnuPlotter();
        gp.setPlotterPath(path);
        gp.setDataFilePath(csvPath);
        gp.setTitle(Utility.getFileName(modelPath));

        gp.setTerminalType(GnuPlotter.TerminalType.valueOf(setting.getTerminalType()));
        gp.setOutputFile(controller.getOutputFilePath());

        if (!setting.isXRangeDefault()) {
            gp.setXmin(setting.getXStart());
            gp.setXmax(setting.getXEnd());
        }

        if (!setting.isYRangeDefault()) {
            gp.setYmin(setting.getYStart());
            gp.setYmax(setting.getYEnd());
        }

        if (!setting.isY2RangeDefault()) {
            gp.setY2min(setting.getY2Start());
            gp.setY2max(setting.getY2End());
        }

        if (!setting.isXLegendDefault()) {
            gp.setXlabel(setting.getXLegend());
        }

        if (!setting.isYLegendDefault()) {
            gp.setYlabel(setting.getYLegend());
        }

        if (!setting.isY2LegendDefault()) {
            gp.setY2label(setting.getY2Legend());
        }

        gp.setKeyDisplayFlg(setting.showsKey());

        ArrayList<Gnuplot> arrData = new ArrayList<>();

        ListItemModel yListModel = setting.getYListItemModel();

        try (FileInputStream fis = new FileInputStream(controller.getIsdFile());
             FileChannel channel = fis.getChannel()) {
            IsdfReader reader = new IsdfReader(channel);
            IsdfHeader header = reader.readHeader();
            int numberOfColumns = header.getNumObjs();
            int skip = header.getDataOffset();

            for (int i = 0; i < yListModel.getSize(); i++) {
                int idx = setting.getTrackIndex(yListModel.keyAt(i)) + 1;
                Gnuplot data = new Gnuplot(arrData.isEmpty() ? isdPath : "",
                                           "1:" + idx,
                                           numberOfColumns,
                                           skip,
                                           yListModel.nameAt(i),
                                           "x1y1",
                                           Gnuplot.GPStyle.lines);
                arrData.add(data);
            }

            ListItemModel y2ListModel = setting.getY2ListItemModel();

            for (int i = 0; i < y2ListModel.getSize(); i++) {
                int idx = setting.getTrackIndex(y2ListModel.keyAt(i)) + 1;
                Gnuplot data = new Gnuplot(arrData.isEmpty() ? isdPath : "",
                                           "1:" + idx,
                                           numberOfColumns,
                                           skip,
                                           y2ListModel.nameAt(i),
                                           "x1y2",
                                           Gnuplot.GPStyle.lines);
                arrData.add(data);
            }
        }

        gp.setDataSets(arrData);
        gp.createConfigFile();
        gp.plotting();
    }

    @Override
    public SwingWorker getWorker(final IChartController controller) {
        return new PlotterWorker(this, controller);
    }

    @Override
    public void preference(final JDialog parent) {
        GnuplotDialog dialog = new GnuplotDialog(parent);
        dialog.setVisible(true);
    }

    @Override
    public String toString() {
        return NAME;
    }
}
