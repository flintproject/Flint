/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.swing.event.EventListenerList;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import org.jfree.data.general.SeriesException;
import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeries;

public class XYProgressSeries extends XYSeries 
    implements ListDataListener {

    private List<Integer> mProgresses;

    private List<Boolean> mIsCancelleds;

    private List<Boolean> mValueIsAdjusting;

    public XYProgressSeries(Comparable key) {
        super(key);

        init();
    }

    public XYProgressSeries (Comparable key, boolean autoSort) {
        super(key, autoSort);

        init();
    }

    public XYProgressSeries (Comparable key, boolean autoSort, 
            boolean allowDuplicateXValues) {
        super(key, autoSort, allowDuplicateXValues);

        init();
    }

    private void init () {
        ContentList contentData = new ContentList();
        contentData.addListDataListener(this);
        this.data = contentData;

        mProgresses = new ArrayList<>();
        mIsCancelleds = new ArrayList<>();
        mValueIsAdjusting = new ArrayList<>();
    }

    public void setValueIsAdjusting(int index, boolean isAdjusting) {
        mValueIsAdjusting.set(index, isAdjusting);
    }

    public boolean getValueIsAdjustring (int index) {
        return mValueIsAdjusting.get(index);
    }

    public int getProgress (int item) {
        return mProgresses.get(item);
    }

    public void setProgress (int item, int progress) {
        if (0<= item && item < mProgresses.size()) {
            mProgresses.set(item, progress);
            fireSeriesChanged();
        }
    }

    public void add (int index, XYDataItem dataItem) {
        add(index, dataItem, true);
    }

    public void add (int index, XYDataItem dataItem, boolean notify) {
      if (dataItem == null)
          throw new IllegalArgumentException("Null 'dataItem' argument.");

      if (!getAllowDuplicateXValues()) {
          if (indexOf(dataItem.getX()) >= 0)
              throw new SeriesException("X-value already exists.");
      }

      this.data.add(index, dataItem);

      if (getItemCount() > getMaximumItemCount()) 
          this.data.remove(0);

      if (notify)
        fireSeriesChanged();
    }

    @Override
    public void intervalAdded(ListDataEvent evt) {
        int start = evt.getIndex0();
        int end   = evt.getIndex1();

        for (int i=start; i<=end; i++) {
            mProgresses.add(i, 0);
            mIsCancelleds.add(i, Boolean.FALSE);
            mValueIsAdjusting.add(i, Boolean.FALSE);
        }
    }

    @Override
    public void intervalRemoved(ListDataEvent evt) {
        int start = evt.getIndex0();
        int end   = evt.getIndex1();

        for (int i=start; i<=end; i++) {
            mProgresses.remove(start);
            mIsCancelleds.remove(start);
            mValueIsAdjusting.remove(start);
        }
    }

    @Override
    public void contentsChanged(ListDataEvent evt) {
    }

    public void onCancelled(int index) {
        mIsCancelleds.set(index, Boolean.TRUE);
    }

    public boolean isCancelled (int index) {
        return mIsCancelleds.get(index);
    }

    public void setCancelled(int index, boolean cancelled) {
        mIsCancelleds.set(index, cancelled);
    }

    public static class ContentList<E> extends ArrayList<E> {

        private EventListenerList mListenerList;

        public ContentList () {
            super();
            mListenerList = new EventListenerList();
        }

        public ContentList (int capacity) {
            super(capacity);
        }

        @Override
        public E set(int index, E e) {
            E ret = super.set(index, e);
            fireContentsChanged(this, index, index);
            return ret;
        }

        @Override
        public boolean add (E e) {
            int index = size();
            boolean success = super.add(e);

            fireIntervalAdded(this, index, index);
            return success;
        }

        @Override
        public void add (int index, E e) {
            super.add(index, e);
            fireIntervalAdded(this, index, index);
        }

        @Override
        public boolean addAll(int index, Collection<? extends E> c) {
            int start = size();
            boolean ret = super.addAll(index, c);
            int end = size()-1;

            fireIntervalAdded(this, start, end);
            return ret;
        }

        @Override
        public E remove (int index) {
            E ret = super.remove(index);
            fireIntervalRemoved(this, index, index);
            return ret;
        }

        @Override
        public void clear () {
            int lastIndex = size() -1;
            super.clear();

            fireIntervalRemoved(this, 0, lastIndex);
        }

        public void addListDataListener (ListDataListener l) {
            mListenerList.add(ListDataListener.class, l);
        }

        public void rmeoveListDataListener (ListDataListener l) {
            mListenerList.remove(ListDataListener.class, l);
        }

        protected void fireContentsChanged(Object source, int startIndex, int endIndex) {
            ListDataEvent event = new ListDataEvent(source, ListDataEvent.CONTENTS_CHANGED,
                                                  startIndex, endIndex);
            ListDataListener[] listeners = (ListDataListener[])mListenerList
                    .getListeners(ListDataListener.class);

            for (ListDataListener l : listeners)
                l.contentsChanged(event);
        }

        protected void fireIntervalAdded(Object source, int startIndex, int endIndex) {
            ListDataEvent event = new ListDataEvent(source, ListDataEvent.INTERVAL_ADDED,
                  startIndex, endIndex);
            ListDataListener[] listeners = (ListDataListener[])mListenerList
                    .getListeners(ListDataListener.class);
      
            for (ListDataListener l : listeners)
                l.intervalAdded(event);
        }

        protected void fireIntervalRemoved(Object source, int startIndex, int endIndex) {
            ListDataEvent event = new ListDataEvent(source, ListDataEvent.INTERVAL_REMOVED,
                                                  startIndex, endIndex);
            ListDataListener[] listeners = (ListDataListener[])mListenerList
                    .getListeners(ListDataListener.class);

            for (ListDataListener l : listeners)
                l.intervalRemoved(event);
        }
    }
}
