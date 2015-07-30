/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.backend;

import com.google.protobuf.ByteString;
import jp.oist.flint.component.Component;
import jp.oist.flint.filesystem.Workspace;
import jp.oist.flint.executor.TimeUnitException;
import jp.physiome.Ipc;
import jp.physiome.Lo;
import jp.physiome.Phml;
import org.apache.log4j.Logger;
import java.io.File;
import java.io.IOException;
import java.io.EOFException;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;
import javax.swing.SwingWorker;

public class ModelLoader extends SwingWorker<Ipc.ModelProbeResponse, Void> {

    private final File mDir;

    private final File mModelFile;

    private final Ipc.ModelProbeResponse.Builder mResponseBuilder;

    public ModelLoader(File modelFile) throws IOException {
        mDir = Workspace.createTempDirectory(modelFile.getName());
        mModelFile = modelFile;
        mResponseBuilder = Ipc.ModelProbeResponse.newBuilder();
    }

    public File getModelFile() {
        return mModelFile;
    }

    public File getDataFile() {
        return new File(mDir, "init");
    }

    public File getParamFile() {
        return new File(mDir, "param");
    }

    @Override
    protected Ipc.ModelProbeResponse doInBackground() throws IOException, InterruptedException, TimeUnitException {
        ProcessBuilder builder = new ProcessBuilder(Component.getOpenCommand());
        builder.directory(mDir);
        Component.setUpEnvironment(builder);
        builder.redirectErrorStream(true);
        Process process = builder.start();
        byte[] bytes = mModelFile.getCanonicalPath().getBytes("UTF-8");
        try (OutputStream os = process.getOutputStream()) {
            os.write(bytes);
        }
        InputStream es = process.getInputStream();
        ByteString message = ByteString.readFrom(es);
        int r = process.waitFor();
        if (r != 0) {
            mResponseBuilder.setStatus(Ipc.ModelProbeResponse.Status.PROCESS_ERROR);
            mResponseBuilder.setErrorMessage(message);
            return mResponseBuilder.build();
        }
        if (!readConfiguration()) {
            mResponseBuilder.setStatus(Ipc.ModelProbeResponse.Status.PROCESS_ERROR);
            mResponseBuilder.setErrorMessage(message);
            return mResponseBuilder.build();
        }
        mResponseBuilder.setStatus(Ipc.ModelProbeResponse.Status.OK);
        return mResponseBuilder.build();
    }

    private boolean readConfiguration() throws IOException, TimeUnitException {
        File file = new File(mDir, "file.txt");
        try (Scanner scanner = new Scanner(file, "UTF8")) {
            String lang = scanner.next();
            switch (lang) {
            case "isml":
            case "phml":
            case "phz":
                mResponseBuilder.setLanguage(Ipc.ModelLanguage.ISML);
                readPhmlConfiguration();
                break;
            case "sbml":
                mResponseBuilder.setLanguage(Ipc.ModelLanguage.SBML);
                readSbmlConfiguration();
                break;
            default:
                mResponseBuilder.setErrorMessage(ByteString.copyFromUtf8("unknown language: " + lang));
                return false;
            }
        }
        return true;
    }

    private void readPhmlConfiguration() throws IOException, TimeUnitException {
        ArrayList<Ipc.TimeUnit> unitoftime = readUnitoftime();
        HashMap<Integer, Ipc.TimeUnit> tus = new HashMap<>();
        Ipc.TimeUnit u0 = null;
        for (Ipc.TimeUnit tu : unitoftime) {
            if (u0 == null) u0 = tu;
            mResponseBuilder.addTimeUnit(tu);
            tus.put(tu.getId(), tu);
        }

        Phml.NumericalConfiguration nc = readNc();
        if (nc.hasTd()) {
            Phml.TimeDiscretization td = nc.getTd();
            mResponseBuilder.setStep(td.getStep());
            mResponseBuilder.setStepUnit(getTimeUnitWithUnitId(tus, td.getUnitId()));
        } else {
            mResponseBuilder.setStep("0.01");
            if (nc.hasSts()) {
                mResponseBuilder.setStepUnit(getTimeUnitWithUnitId(tus, nc.getSts().getUnitId()));
            } else if (u0 != null) {
                mResponseBuilder.setStepUnit(u0);
            } else {
                // TODO
            }
        }
        if (nc.hasRg() && nc.getRg().hasSeed()) {
            Phml.RandomGenerator rg = nc.getRg();
            int seed = Integer.parseInt(rg.getSeed());
            mResponseBuilder.setSeed(seed);
        }
        if (nc.hasIntegration()) {
            switch (nc.getIntegration()) {
            case "euler":
                mResponseBuilder.setIntegrationMethod(Ipc.IntegrationMethod.EULER);
                break;
            case "4th-rungekutta":
                mResponseBuilder.setIntegrationMethod(Ipc.IntegrationMethod.RUNGE_KUTTA);
                break;
            }
        }
        if (nc.hasSts()) {
            Phml.SimulationTimeSpan sts = nc.getSts();
            mResponseBuilder.setLength(sts.getValue());
            mResponseBuilder.setLengthUnit(getTimeUnitWithUnitId(tus, sts.getUnitId()));
        } else {
            mResponseBuilder.setLength("100");
            if (nc.hasTd()) {
                mResponseBuilder.setLengthUnit(getTimeUnitWithUnitId(tus, nc.getTd().getUnitId()));
            } else if (u0 != null) {
                mResponseBuilder.setLengthUnit(u0);
            } else {
                // TODO
            }
        }

        mResponseBuilder.setVariableTable(buildModelVariableTableForPhml());
    }

    private ArrayList<Ipc.TimeUnit> readUnitoftime() throws IOException {
        ArrayList<Ipc.TimeUnit> result = new ArrayList<>();
        File file = new File(mDir, "unitoftime");
        try (DataInputStream dis = new DataInputStream(new FileInputStream(file))) {
        for (;;) {
            int len;
            try {
                len = dis.readInt();
            } catch (EOFException eofe) {
                break;
            }
            if (len <= 0) { // TODO
                Logger.getRootLogger().error("invalid length of Ipc.TimeUnit: " + len);
                break;
            }
            byte[] buf = new byte[len];
            dis.readFully(buf);
            result.add(Ipc.TimeUnit.parseFrom(buf));
        }
        }
        return result;
    }

    private Phml.NumericalConfiguration readNc() throws IOException {
        File file = new File(mDir, "nc");
        try (FileInputStream in = new FileInputStream(file)) {
            return Phml.NumericalConfiguration.parseFrom(in);
        }
    }

    private Ipc.TimeUnit getTimeUnitWithUnitId(HashMap<Integer, Ipc.TimeUnit> tus, int unitId) throws TimeUnitException {
        Ipc.TimeUnit tu = tus.get(unitId);
        if (tu == null) throw new TimeUnitException("could not find the valid time unit with unit-id " + unitId);
        return tu;
    }

    private Ipc.ModelVariableTable buildModelVariableTableForPhml() throws IOException {
        Ipc.ModelVariableTable.Builder builder = Ipc.ModelVariableTable.newBuilder();
        builder.addColumn("Physical Quantity Name");
        builder.addColumn("Module Name");

        File file = new File(mDir, "var");
        try (DataInputStream dis = new DataInputStream(new FileInputStream(file))) {
        int len;
        byte[] buf;

        len = dis.readInt();
        if (len <= 0) { // TODO
            Logger.getRootLogger().error("invalid length of Lo.Header: " + len);
            // FIXME
        }
        buf = new byte[len];
        dis.readFully(buf);
        Lo.Header header = Lo.Header.parseFrom(buf);

        for (;;) {
            try {
                len = dis.readInt();
            } catch (EOFException eofe) {
                break;
            }
            if (len <= 0) { // TODO
                Logger.getRootLogger().error("invalid length of Lo.Column: " + len);
                break;
            }
            buf = new byte[len];
            dis.readFully(buf);
            Lo.Column column = Lo.Column.parseFrom(buf);
            Ipc.ModelVariable.Builder mvBuilder = Ipc.ModelVariable.newBuilder();
            String pqName = column.getName();
            String moduleName = column.hasTrackName() ? column.getTrackName() : "-";
            mvBuilder.setKey(column.getUuid().toStringUtf8()+" "+pqName).addValue(pqName).addValue(moduleName);
            builder.addVariable(mvBuilder.build());
        }
        }

        return builder.build();
    }

    private void readSbmlConfiguration() throws IOException {
        mResponseBuilder.setIntegrationMethod(Ipc.IntegrationMethod.EULER);
        mResponseBuilder.setLength("100");
        mResponseBuilder.setStep("0.01");

        Ipc.TimeUnit.Builder tuBuilder = Ipc.TimeUnit.newBuilder();
        tuBuilder.setName("unit time");
        tuBuilder.setD(1);
        tuBuilder.setN(1);
        mResponseBuilder.addTimeUnit(tuBuilder.build());

        mResponseBuilder.setVariableTable(buildModelVariableTableForSbml());
    }

    private Ipc.ModelVariableTable buildModelVariableTableForSbml() throws IOException {
        Ipc.ModelVariableTable.Builder builder = Ipc.ModelVariableTable.newBuilder();
        builder.addColumn("Variable Name");
        builder.addColumn("Module Name");

        File file = new File(mDir, "var");
        try (DataInputStream dis = new DataInputStream(new FileInputStream(file))) {
        int len;
        byte[] buf;

        len = dis.readInt();
        if (len <= 0) { // TODO
            Logger.getRootLogger().error("invalid length of Lo.Header: " + len);
            // FIXME
        }
        buf = new byte[len];
        dis.readFully(buf);
        Lo.Header header = Lo.Header.parseFrom(buf);

        for (;;) {
            try {
                len = dis.readInt();
            } catch (EOFException eofe) {
                break;
            }
            if (len <= 0) { // TODO
                Logger.getRootLogger().error("invalid length of Lo.Column: " + len);
                break;
            }
            buf = new byte[len];
            dis.readFully(buf);
            Lo.Column column = Lo.Column.parseFrom(buf);
            Ipc.ModelVariable.Builder mvBuilder = Ipc.ModelVariable.newBuilder();
            String pqName = column.getName();
            String moduleName = column.hasTrackName() ? column.getTrackName() : "-";
            mvBuilder.setKey(column.getUuid().toStringUtf8()+" "+pqName).addValue(pqName).addValue(moduleName);
            builder.addVariable(mvBuilder.build());
        }
        }

        return builder.build();
    }
}
