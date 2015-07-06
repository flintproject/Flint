/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.filesystem;

import org.apache.log4j.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import static java.nio.file.StandardWatchEventKinds.*;

public class ModelFileWatcher implements Runnable {

    private final WatchService mService;

    private final Map<Path, Map<Path, IModelFileClient> > mDirMap;
    private final Map<Path, WatchKey> mKeyMap;

    public ModelFileWatcher() throws IOException {
        mService = FileSystems.getDefault().newWatchService();
        mDirMap = Collections.synchronizedMap(new HashMap<Path, Map<Path, IModelFileClient> >());
        mKeyMap = Collections.synchronizedMap(new HashMap<Path, WatchKey>());
    }

    public void watch(File file, IModelFileClient client) throws IOException {
        Path dirname = file.getParentFile().toPath();
        Path basename = Paths.get(file.getName());
        Map<Path, IModelFileClient> m = mDirMap.get(dirname);
        if (m == null) {
            WatchKey key = dirname.register(mService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE);
            m = new HashMap<>();
            m.put(basename, client);
            mDirMap.put(dirname, m);
            mKeyMap.put(dirname, key);
        } else {
            m.put(basename, client);
        }
    }

    public void unwatch(File file) {
        Path dirname = file.getParentFile().toPath();
        Path basename = Paths.get(file.getName());
        Map<Path, IModelFileClient> m = mDirMap.get(dirname);
        if (m == null) return; // nothing to do
        m.remove(basename);
        if (m.isEmpty()) {
            mDirMap.remove(dirname);
            WatchKey key = mKeyMap.get(dirname);
            if (key == null) return; // nothing to do
            key.cancel();
        }
    }

    @Override
    public void run() {
        try {
            for (;;) {
                WatchKey key = mService.take();
                for (WatchEvent<?> event : key.pollEvents()) {
                    WatchEvent.Kind<?> kind = event.kind();
                    if (kind == OVERFLOW) {
                        continue;
                    }
                    WatchEvent<Path> ev = cast(event);
                    Path dirname = (Path)key.watchable();
                    Path basename = ev.context();
                    Map<Path, IModelFileClient> m = mDirMap.get(dirname);
                    if (m == null) continue;
                    IModelFileClient client = m.get(basename);
                    if (client == null) continue;
                    if (kind != ENTRY_DELETE) {
                        client.notifyModelFileModified();
                        break;
                    }
                }
                boolean b = key.reset();
                if (!b) {
                    continue; // TODO: remove invalid key from the map?
                }
            }
        } catch (InterruptedException e) {
            Logger.getRootLogger().error(e.getMessage());
        }
    }

    // suppress a warning:
    // Note: ....java uses unchecked or unsafe operations.
    // Note: Recompile with -Xlint:unchecked for details.
    // Cf. http://docs.oracle.com/javase/tutorial/essential/io/notification.html
    @SuppressWarnings("unchecked")
    private static <T> WatchEvent<T> cast(WatchEvent<?> event) {
        return (WatchEvent<T>)event;
    }
}
