/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.util;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.IllegalComponentStateException;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.InputMap;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;

public class AutoCompleter implements 
        FocusListener, MouseListener, ComponentListener, KeyListener {

    public final static int LEFT_INDEX = 0;

    public final static int RIGHT_INDEX = 1;

    private final static String DISABLE_ACTION_MAP = "none";

    private final static long WAIT_TIME_FOR_POPUP = 1000; // 1 sec

    private final HashMap<Integer, Object> mEventKeyDefaultActionMaps;

    private final int[] mEventKeyCodes = new int[] {
        KeyEvent.VK_UP, KeyEvent.VK_DOWN, KeyEvent.VK_ENTER, KeyEvent.VK_TAB
    };

    private final static List<String> mIgnoreWords = Arrays.asList (new String[] {
        "+", "-", "*", "/", "=",
        ">", "<", "!", "?", ",",
        "(", ")", "{", "}", 
        " ", "\t", System.getProperty("line.separator")
    });

    private final JTextComponent mTextComponent;

    private Completer mCompleter;
    private JList mCompletionList;
    private JScrollPane mListScroller;
    private final JWindow mListWindow;

    private boolean mIsAutComplecaterEnabled = true;

    public AutoCompleter (JTextComponent textComponent) {
        mTextComponent = textComponent;
        mEventKeyDefaultActionMaps = new HashMap<>();

        mListWindow = new JWindow();

        initComponents();
    }

    private void initComponents () {
        mCompleter = new Completer();        
        mCompletionList = new JList(new DefaultListModel()); 

        InputMap inputMap = getInputMap(JTextComponent.WHEN_FOCUSED);

        KeyStroke ks;
        for (int keyCode : mEventKeyCodes) {
            ks = KeyStroke.getKeyStroke(keyCode, 0);
            mEventKeyDefaultActionMaps.put(keyCode, inputMap.get(ks));
        }
        mTextComponent.addCaretListener(mCompleter);
        mTextComponent.getDocument().addDocumentListener(mCompleter);
        mTextComponent.addPropertyChangeListener(new PropertyChangeListener () {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                String propertyName = evt.getPropertyName();
                JTextComponent txtComponent = (JTextComponent)evt.getSource();
                if ("Frame.active".equals(propertyName) || "ancestor".equals(propertyName)) {
                    if (txtComponent.isShowing() == false)
                        mListWindow.setVisible(false);
                }
            }
        });
        mTextComponent.addKeyListener(this);

        mCompletionList.setSelectionMode (ListSelectionModel.SINGLE_SELECTION); 
        mCompletionList.setOpaque(true);
        mCompletionList.addMouseListener(this);
        mCompletionList.addKeyListener(this);
        mCompletionList.setSelectionForeground(Color.white);
        mCompletionList.setCellRenderer(new AutoCompleterListCellRenderer());


        mListScroller = new JScrollPane(mCompletionList, 
                 ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
                 ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        mListScroller.setOpaque(true);

        JPanel contentPane = new JPanel(new BorderLayout());
        contentPane.add(mListScroller, BorderLayout.CENTER);
        contentPane.setOpaque(true);

        mListWindow.addComponentListener(this);
        mListWindow.setContentPane(contentPane);
    }

    public void setForeground (Color c) {
        mCompletionList.setForeground(c);
    }

    public void setBackground (Color c) {
        mCompletionList.setBackground(c);
    }

    public void setSelectionForeground (Color c) {
        mCompletionList.setSelectionForeground(c);
    }

    public void setSelectionBackground (Color c) {
        mCompletionList.setSelectionBackground(c);
    }

    public InputMap getInputMap (int condition) {
        return mTextComponent.getInputMap(condition);
    }

    public Document getDocument() {
        return mTextComponent.getDocument();
    }

    public String getText() {
        return mTextComponent.getText();
    }

    public void setText(String t) {
        setAutoCompleterEnabled(false);
        mTextComponent.setText(t);
        setAutoCompleterEnabled(true);
    }

    public Caret getCaret () {
        return mTextComponent.getCaret();
    }

    public int getCaretPosition() {
        return mTextComponent.getCaretPosition();
    }

    public void setCaretPosition (int position) {
        mTextComponent.setCaretPosition(position);
    }

    public Rectangle modelToView (int pos) throws BadLocationException {
        return mTextComponent.modelToView(pos);
    }

    public void scrollRectToVisible (Rectangle aRect) {
        mTextComponent.scrollRectToVisible(aRect);
    }

    public Point getLocationOnScreen () {
        return mTextComponent.getLocationOnScreen();
    }

    public void add (String left, String right) {
        mCompleter.add(left, right);
    }

    public void set (int index, String left, String right) {
        mCompleter.set(index, left, right);
    }

    public void remove (int index) {
        mCompleter.remove(index); 
    }

    public void remove (String left) {
        mCompleter.remove(left);
    }

    public void clear () {
        mCompleter.clear();
    }

    public String getCaretPositionWord () {
        int curPos = getCaretPosition(); 
        return getWord(curPos);
    }

    public String getWord (int pos) {
        String retval = "";

        if (getText().isEmpty()) return retval;

        String text = getText();

        StringRange range = getWordRange(pos);
        retval = text.substring(range.getBegin(), range.getEnd());

        return retval;
    }

    public void setAutoCompleterEnabled (boolean b) {
        mIsAutComplecaterEnabled = b;
    }

    public void setListCellRenderer (ListCellRenderer cellRenderer) {
        mCompletionList.setCellRenderer(cellRenderer);
    }

    public ListCellRenderer getListCellRenderer () {
        return mCompletionList.getCellRenderer();
    }

    protected List<Map> buildCompletionWords (List<String[]> completionWords, String targetWord) {
        ArrayList<Map> retval = new ArrayList<>();

        boolean isMatched = false;
        for (String[] completion : completionWords) {
            String leftWord = completion[LEFT_INDEX]; 
            String rightWord = completion[RIGHT_INDEX];

            HashMap<String, String> data = 
                    new HashMap<>();
            if (leftWord.startsWith(targetWord)
                    && !leftWord.equals(targetWord)) {
                data.put("left",  leftWord);
                data.put("right", rightWord);
                retval.add(data);
            }
        }

        Collections.sort(retval, new Comparator<Map> () {
            @Override
            public int compare(Map map1, Map map2) {
                String word1 = (String)map1.get("left");
                String word2 = (String)map2.get("left");

                return word1.compareTo(word2);
            }
        });

        return retval;
    }

    private StringRange getWordRange (int pos) {
        String text = getText();
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (String word : mIgnoreWords) {
            sb.append("\\").append(word);
        }
        sb.append("]");
        String ignoreRegex = sb.toString();

        int textLength = text.length();

        Pattern p = Pattern.compile(ignoreRegex);
        Matcher m = p.matcher(text);

        int beginIndex=-1, endIndex=-1;

        if (pos > text.length()) pos = text.length();

        m.region(0, pos);
        while (m.find()) { 
            beginIndex = m.end();
        } 
        if (beginIndex == -1) beginIndex = 0;

        m.region(pos, textLength);
        while (m.find()) {
            endIndex = m.start();
            break;
        }
        if (endIndex == -1) endIndex = textLength;

        return new StringRange(beginIndex, endIndex);
    }

    public Point getPredictCaretPosition() {
        Point retval;
        try {
            Point p = getCaret().getMagicCaretPosition();
            if (p!=null) return p;

            int dot = getCaret().getDot();
            Rectangle area = modelToView(dot);
            scrollRectToVisible(area);
            retval = new Point(area.x, area.y);
            getCaret().setMagicCaretPosition(retval);
        } catch (BadLocationException ex) {
            retval = new Point(0, 0);
        }
        return retval;
    }

    public void setCompletionVisible (boolean b) {
        mListWindow.setVisible(b);
    }

    public boolean isAutoCompleterVisible() {
        return mListWindow.isVisible();
    }

    public void setAutoCompleterVisible(boolean b) {
        mListWindow.setVisible(b);
    }

    public void upSelectedPredict (boolean looping) {
        int selectedIndex = mCompletionList.getSelectedIndex();
        int lastIndex = mCompletionList.getModel().getSize()-1;
        int index;
        if (selectedIndex > 0) {
            index = selectedIndex-1;
        } else if(looping) {
            index = lastIndex;
        } else {
            index = selectedIndex; // index is 0
        }
        mCompletionList.setSelectedIndex(index);
    }

    public void downSelectedPredict (boolean looping) {
        int selectedIndex = mCompletionList.getSelectedIndex();
        int lastIndex = mCompletionList.getModel().getSize()-1;
        int firstIndex = 0;
        int index;
        if (lastIndex > selectedIndex) {
            index = selectedIndex+1;
        } else if(looping) {
            index = firstIndex;
        } else {
            index = selectedIndex; // index is last index
        }
        mCompletionList.setSelectedIndex(index);
    }

    public void clickSelectedPredict () {
        int si = mCompletionList.getSelectedIndex();
        Point p = mCompletionList.indexToLocation(si);
        MouseEvent me = new MouseEvent(mCompletionList,  // which
                MouseEvent.MOUSE_CLICKED,     // what 
                System.currentTimeMillis(),   // when
                0,          // no modifiers
                p.x, p.y,   // where 
                1,          // single click
                false);     // not a popup trigger
        MouseListener[] listeners = mCompletionList.getMouseListeners();
        for (MouseListener l : listeners) {
            l.mouseClicked(me);
        }
    }

    /*
     * implement FocusListener on mCompletionList
     */
    @Override
    public void focusLost(FocusEvent e) {
        mListWindow.setVisible(false);
    }

    @Override
    public void focusGained(FocusEvent e) { }

    /*
     *  implement MouseListener on mCompletionList
     */
    @Override
    public void mouseClicked(MouseEvent e) {
        int selectedIndex = mCompletionList.getSelectedIndex();

        if (selectedIndex < 0) return;

        Map vars = (Map)mCompletionList.getSelectedValue();

        String completionWord = (String)vars.get("left");

        String text = getText();
        int textLength = text.length();
        int curPos = getCaretPosition(); 
        StringRange range = getWordRange(curPos);

        String beginText = text.substring(0, range.getBegin());
        String endText   = text.substring(range.getEnd(), textLength); 

        String newText = new StringBuilder(beginText)
                .append(completionWord)
                .append(endText)
                .toString();

        String oldWord = text.substring(range.getBegin(), range.getEnd());

        int gap = completionWord.length() - oldWord.length();

        setText(newText);
        setCaretPosition(curPos+gap);

        mListWindow.setVisible (false);
    }

    @Override 
    public void mousePressed(MouseEvent e) { 
    }

    @Override 
    public void mouseReleased(MouseEvent e) { 
    }

    @Override
    public void mouseEntered(MouseEvent e) { 
    }

    @Override 
    public void mouseExited(MouseEvent e) { 
    }

    /*
     * implement ComopnentListener on mListWindow
     */
    @Override
    public void componentShown(ComponentEvent e) {
        InputMap inputMap = getInputMap(JTextComponent.WHEN_FOCUSED);
        // activate event key
        for (int keyCode : mEventKeyCodes) {
            KeyStroke ks = KeyStroke.getKeyStroke(keyCode, 0);
            inputMap.put(ks, DISABLE_ACTION_MAP);
        }
    }

    @Override
    public void componentHidden(ComponentEvent e) {
        InputMap inputMap = getInputMap(JTextComponent.WHEN_FOCUSED);
        // disactivate event key
        for (int keyCode : mEventKeyCodes) {
            KeyStroke ks = KeyStroke.getKeyStroke(keyCode, 0);
            inputMap.put(ks, mEventKeyDefaultActionMaps.get(keyCode));
        }
    }

    @Override
    public void componentResized(ComponentEvent e) { 
    }

    @Override 
    public void componentMoved(ComponentEvent e) { 
    }

    /*
     *  implement KeyListener on mTextComponent
     */
    @Override
    public void keyTyped(KeyEvent evt) {
        boolean looping = false;
        Component c = (Component)evt.getSource();
        if (isAutoCompleterVisible()) {
            int keyCode = evt.getKeyCode();
            switch (keyCode) {
                case KeyEvent.VK_TAB:
                case KeyEvent.VK_ENTER:
                    clickSelectedPredict();
                    break;
                case KeyEvent.VK_UP:
                    if (c instanceof JList == false)
                        upSelectedPredict(looping);
                    break;

                case KeyEvent.VK_DOWN:
                    if (c instanceof JList == false)
                        downSelectedPredict(looping);
                    break;
            }
        }
        mCompletionList.ensureIndexIsVisible(
                mCompletionList.getSelectedIndex());
    }

    @Override
    public void keyReleased(KeyEvent evt) {
        keyTyped(evt);
    }

    @Override
    public void keyPressed(KeyEvent evt) { 
    }

    private static class Cell extends JPanel 
            implements PropertyChangeListener {

        private JLabel mLeftLabel;
        private JLabel mRightLabel;

        public Cell () {
            super(new BorderLayout());

            initComponents();
        }

        private void initComponents () {
            mLeftLabel = new JLabel();
            add(mLeftLabel, BorderLayout.WEST);

            mRightLabel = new JLabel();
            add(mRightLabel, BorderLayout.EAST);

            addPropertyChangeListener(this);
        }

        public void setLeftWord (String s) {
            mLeftLabel.setText(s);
        }

        public String getLeftWord () {
            return mLeftLabel.getText();
        }

        public void setRightWord (String s) {
            mRightLabel.setText(s);
        }

        public String getRightWord () {
            return mRightLabel.getText();
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            String propertyName = evt.getPropertyName();
            if (propertyName == null)
                return;
            Object newValue = evt.getNewValue();
            switch (propertyName) {
            case "foreground":
                mLeftLabel.setForeground((Color)newValue);
                mRightLabel.setForeground((Color)newValue);
                break;
            case "background":
                mLeftLabel.setBackground((Color)newValue);
                mRightLabel.setBackground((Color)newValue);
                break;
            }
        }
    }

    private static class AutoCompleterListCellRenderer
        extends DefaultListCellRenderer {

        @Override
        public Component getListCellRendererComponent(JList list, Object value,
                    int index, boolean isSelected, boolean cellHasFocus) {
            Map<String, String> data = (Map<String, String>) value;
            String word = data.get("left");
            String memo = data.get("right");

            Cell cell = new Cell();
            cell.setLeftWord(word);
            cell.setRightWord(memo);


            if (isSelected) {
                cell.setForeground(list.getSelectionForeground());
                cell.setBackground(list.getSelectionBackground());
            } else {
                cell.setForeground(list.getForeground());
                cell.setBackground(list.getBackground());
            }

            return cell;
        }
    }


    private static class StringRange {

        private final int begin;
        private final int end;

        public StringRange (int begin, int end) {
            this.begin = begin;
            this.end = end;
        }

        public int getBegin() {
            return this.begin;
        }

        public int getEnd () {
            return this.end;
        }
    }

    /*
     * Inner Class Completer
     */
    private class Completer implements CaretListener, DocumentListener {

        private final ArrayList<String[]> mCompletions;

        private Thread mWaitThread = null;

        private boolean mIsCaretUpdateEnabled = false;

        public Completer() {
            mCompletions = new ArrayList<>();
        }

        public String[] get(int index) {
            return mCompletions.get(index);
        }
        
        public void add (String left, String right) {
            String[] data = new String[2];
            data[LEFT_INDEX] = left;
            data[RIGHT_INDEX] = right;
            mCompletions.add(data);
        }

        private void set(int index, String left, String right) {
            String[] data = mCompletions.get(index);
            data[LEFT_INDEX] = left;
            data[RIGHT_INDEX] = right;
            mCompletions.set(index, data);
        }

        public void remove (int i) {
            mCompletions.remove(i);
        }

        public void remove (String left) {
            for (int i=0; i<mCompletions.size(); i++) {
                if (mCompletions.get(i)[LEFT_INDEX].equals(left)) {
                    mCompletions.remove(i);
                    break;
                }
            }
        }

        public void clear () {
            mCompletions.clear();
            buildPopup(0);
            mListWindow.setVisible(false);
        }

        public void buildAndShowPopup(int offset) {
            if (mIsAutComplecaterEnabled) {
                buildPopup(offset);
                showPopup();
            }
        }

        private void buildPopup(int offset) {
            DefaultListModel model = (DefaultListModel)mCompletionList.getModel();
            model.clear();
            String targetWord = getCaretPositionWord();

            if (targetWord.isEmpty()) return;

            List<Map> possibleCompletionWords = buildCompletionWords(mCompletions, targetWord);
            for (Map map : possibleCompletionWords)
                model.addElement(map);

            if (model.getSize() > 0)
                mCompletionList.setSelectedIndex(0);
        }

        private void setCaareteUpdateEnabled (boolean b) {
            mIsCaretUpdateEnabled = b;
        }

        private boolean isCaareteUpdateEnabled () {
            return mIsCaretUpdateEnabled;
        }

        private void showPopup() {
            try {
                DefaultListModel model = (DefaultListModel)mCompletionList.getModel();
                if (model.getSize() == 0) { 
                    mListWindow.setVisible(false); 
                    return;
                }
                Point absPoint    = getLocationOnScreen();
                Point caretPoint = getCaret().getMagicCaretPosition();
                if (caretPoint == null) caretPoint = getPredictCaretPosition();

                int cursorX = (caretPoint==null)? 0:caretPoint.x;
                int cursorY = (caretPoint==null)? 0:caretPoint.y;

                int popX = absPoint.x+cursorX;
                int popY = absPoint.y+cursorY+20;

                mListWindow.setLocation (popX, popY);
                mListWindow.pack();

                int w = mListWindow.getWidth();
                int h = mListWindow.getHeight();
                mListWindow.setSize(w+100, h);

                mListWindow.setVisible(true);

            } catch (IllegalComponentStateException ex) {
                mListWindow.setVisible(false);
            }
        }

        /**
         * implement CaretListener on mTextComponent
         */
        @Override
        public void caretUpdate(CaretEvent e) {
            if (!isCaareteUpdateEnabled()) {
                setCaareteUpdateEnabled(true);
                return;
            }
            mListWindow.setVisible(false);
        }

        /**
         * implement DocumentListener on mTextComponent
         */            
        @Override
        public void insertUpdate (DocumentEvent e) { 
            setCaareteUpdateEnabled(false);
            documentUpdate(e);
        }

        @Override
        public void removeUpdate (DocumentEvent e) { 
            setCaareteUpdateEnabled(false);
            documentUpdate(e);
        }

        @Override
        public void changedUpdate (DocumentEvent e) { 
        }

        private void documentUpdate (DocumentEvent e) {
            final int changeOffset = e.getOffset();
            if (!mListWindow.isVisible()) {
                mListWindow.setVisible(false);
                if (mWaitThread != null) { 
                    try {
                        mWaitThread.interrupt();
                        mWaitThread.join();
                    } catch (InterruptedException ex) { 
                        // ignore
                    }
                }
                mWaitThread = new Thread() {
                    @Override
                    public void run () {
                        try {
                            Thread.sleep(WAIT_TIME_FOR_POPUP);
                            buildAndShowPopup(changeOffset);
                        } catch (InterruptedException ex1) { 
                            // ignore
                        }
                    }
                };
                mWaitThread.start();
            } else {
                buildAndShowPopup(changeOffset);
            }
        }
    } 
}
