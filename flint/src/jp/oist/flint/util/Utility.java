/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.util;

import jp.oist.flint.control.VariableList;
import java.awt.Component;
import java.awt.Container;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import javax.swing.DropMode;
import javax.swing.JComponent;
import javax.swing.ListSelectionModel;
import javax.swing.TransferHandler;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import net.arnx.jsonic.util.Base64;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * This is the class which collects utility functions for general use.
 */
public class Utility {

    private final static byte[] KEY = new byte[] {
        0x43, 0x54, 0x30, 0x73, 0x41, 0x4f, 0x38, 0x6d, 
        0x34, 0x33, 0x59, 0x49, 0x45, 0x6d, 0x64, 0x67, 
    };

    public static Number s2num (String s) {

        try {
            return Long.parseLong(s);
        } catch (NumberFormatException ex) { }

        try {
            return Double.parseDouble(s);
        } catch (NumberFormatException ex) {}

        return null;
    }

    /**
     * The file name of the specified file path is returned.
     *
     * @param filePath A file path to return a file name to.
     * @return String file name.
     */
    public static String getFileName(String filePath) {
        File file = new File(filePath);
        String fileName = file.getName();
        int postion = fileName.lastIndexOf(".");
        if (postion == -1) return fileName;
        return fileName.substring(0, postion);
    }

    /**
     * Get the file extension of given file.
     *
     * @param file A given file.
     * @return String the file extension if any, otherwise null.
     */
    public static String getFileExtension(File file) {
        String fileName = file.getName();
        int position = fileName.lastIndexOf(".");
        if (position == -1) return null;
        return fileName.substring(position+1);
    }

    public static HashTableModel convertVariableTableToTableModel(jp.physiome.Ipc.ModelVariableTable table) { 
        HashMap tableHeader = new HashMap();
        List<Map> tableData = new ArrayList<>();

        int c = table.getColumnCount();
        for (int i=0;i<c;i++) {
            tableHeader.put(i, table.getColumn(i));
        }
        tableHeader.put(c, "key"); // key

        for (jp.physiome.Ipc.ModelVariable variable : table.getVariableList()) {
            HashMap v = new HashMap();
            for (int i=0; i<variable.getValueCount(); i++) {
                v.put(tableHeader.get(i), variable.getValue(i));
            }
            v.put("key", variable.getKey());
            tableData.add(v);
        }

        HashTableModel htm = new HashTableModel(tableData, tableHeader);

        return htm;
    }

    /**
     * The Enable attribute of Component in specified JComponent is changed.
     *
     * @param component Parents' JComponent.
     * @param enabled Enable attribute.
     */
    public static void setEnabledJComponent(JComponent component, boolean enabled) {
        Component[] children = component.getComponents();
        for (int i = 0; i < children.length; ++i) {
            if (children[i] instanceof JComponent) {
                setEnabledJComponent((JComponent) children[i], enabled);
            }
        }
        component.setEnabled(enabled);
    }

    /**
     * VariableList is returned based on track.
     *
     * @param name VariableList Component Name.
     * @param track Simulation track list.
     * @param handler TransferHandler Object.
     * @return VariableList
     */
    public static VariableList makeVariableList(String name, LinkedHashMap<String, String> track, TransferHandler handler) {
        return makeVariableList(name, track, handler, true);
    }

    /**
     * VariableList is returned based on track.
     *
     * @param name VariableList Component Name.
     * @param track Simulation track list.
     * @param handler TransferHandler Object.
     * @param multiple Multiple selection flag.
     * @return VariableList
     */
    public static VariableList makeVariableList(String name, LinkedHashMap<String, String> track, TransferHandler handler, boolean multiple) {
        ListItemModel listModel = new ListItemModel(track);
        listModel.setMultipleFlg(multiple);

        VariableList list = new VariableList(listModel);

        if (multiple) {
            list.getSelectionModel().setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        } else {
            list.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        }

        list.setName(name);
        list.setDropMode(DropMode.INSERT);
        list.setDragEnabled(true);
        list.setTransferHandler(handler);

        return list;
    }

    /**
     * The numerical value below a significant figure is rounded off.
     *
     * @param value The target value
     * @param effectiveDigit Significant figure digit number
     * @return retVal The rounded-off numerical value
     */
    public static String getEfficientRound(BigDecimal val, int effectiveDigit) {

        if (effectiveDigit <= 0) {
            return val.toString();
        }

        int intLength = getIntLength(val);
        int delta = intLength - effectiveDigit;

        if (delta > 0) {
            BigDecimal tenPower = new BigDecimal(new BigInteger("10").pow(delta));
            BigDecimal unscaledValue = new BigDecimal(val.unscaledValue());
            BigDecimal bak = unscaledValue.divide(tenPower, 0, BigDecimal.ROUND_HALF_UP).multiply(tenPower);

            BigDecimal tempVal = new BigDecimal(bak.unscaledValue(), val.scale());
            StringBuilder tempStr = new StringBuilder(tempVal.toString());
            StringBuilder reverseStr = new StringBuilder(tempStr.reverse().substring(delta));
            String retVal = reverseStr.reverse().toString();

            if (retVal.substring(retVal.length() - 1).equals(".")) {
                return retVal.replace(".", "");
            }

            return retVal;
        } else {
            return val.toString();
        }
    }

    /**
     * The digit number of an integer part is acquired.
     *
     * @param val The target value
     * @return length The digit number of an integer portion
     */
    private static int getIntLength(BigDecimal val) {
        BigInteger intVal = val.unscaledValue();
        intVal = intVal.abs();
        String bak = intVal.toString();

        return bak.length();
    }

    public static List<Component> findComponentsByClass (Container parent, 
            Class<? extends Component> targetClass) {

        ArrayList<Component> retval = new ArrayList<>();
        for (Component c : parent.getComponents()) {
            if (targetClass.isInstance(c))
                retval.add(c);

            if (c instanceof Container)
                retval.addAll(findComponentsByClass((Container)c, targetClass));
        }
        return retval;
    }

    public static String detectXMLFormat (File f) {
        DetectionXMLFormatHandler handler = new DetectionXMLFormatHandler();
        SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware(true);
        factory.setValidating(false);
        try {
            SAXParser parser = factory.newSAXParser();
            parser.parse(f, handler);
        } catch (BreakException ex) {
            return handler.format;
        } catch (ParserConfigurationException | SAXException | IOException ex) { }
        return "";
    }

    public static int getDecimalPlace(Number number) {
        if (number == null)
            return -1;

        if (number instanceof Integer || number instanceof Long 
                || number instanceof BigInteger)
            return 0;

        String snum;
        snum = String.valueOf(number);

        int index = snum.indexOf('.');

        return snum.substring(index+1).length();
    }

    public static int getDecimalPlace (String sNumber) {
        try{ 
           Long.parseLong(sNumber);
            return 0;
        } catch (NumberFormatException ex) { }

        try{ 
            Double.parseDouble(sNumber);
            int index = sNumber.indexOf('.');
            return sNumber.substring(index+1).length();
        } catch (NumberFormatException ex) { }

        return -1;
    }

    public static String encrypt(String str) throws GeneralSecurityException {
            Cipher cipher  = Cipher.getInstance("AES");
            cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(KEY, "AES"));
            byte[] encryptData = cipher.doFinal(str.getBytes());

            return Base64.encode(encryptData);
    }

    public static String decrypt (String encryptStr) throws GeneralSecurityException {
            byte[] encryptData = Base64.decode(encryptStr);
            Cipher cipher  = Cipher.getInstance("AES");
            cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(KEY, "AES"));
            byte[] data = cipher.doFinal(encryptData);

            return new String(data);
    }

    private static class BreakException extends SAXException { 
        public BreakException () {}
    }

    private static class DetectionXMLFormatHandler extends DefaultHandler {
        public String format = null;
        @Override
        public void startElement (String namespaceUri, String localName, 
                String qName, Attributes attrs) 
            throws BreakException {
            if (localName.equals("insilico-model")) {
                format = "phml";
                throw new BreakException();
            }

            if (localName.equals("sbml")) {
                format = "sbml";
                throw new BreakException();
            }

            if (localName.equals("phsp")) {
                format = "phsp";
                throw new BreakException();
            }
        }
    }
}
