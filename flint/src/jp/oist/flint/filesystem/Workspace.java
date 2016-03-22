/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.filesystem;

import org.apache.log4j.Logger;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.UUID;

public class Workspace {

    private static final String LOCK_FILE_NAME = "flint.lock";

    private final static String WORKSPACE_NAME;

    static {
        WORKSPACE_NAME = new File(".flint", UUID.randomUUID().toString()).getPath();

        File dir = getFile();
        if (!dir.mkdirs()) {
            Logger.getRootLogger().fatal("failed to create workspace directory: " + dir.toString());
            System.exit(1);
        }
        dir.deleteOnExit();
        File lockFile = new File(dir, LOCK_FILE_NAME);
        try {
            if ( !lockFile.exists() &&
                 !lockFile.createNewFile() ) {
                Logger.getRootLogger().fatal("failed to create lock file: " + lockFile.toString());
                System.exit(1);
            }
            lockFile.deleteOnExit();
            LockThread lockThread = new LockThread(lockFile);
            lockThread.start();

            clear();
        } catch (IOException ioe) {
            Logger.getRootLogger().fatal(ioe.getMessage());
            System.exit(1);
        }

        Runtime.getRuntime().addShutdownHook(new ShutdownHook(dir, lockFile));
    }

    private static void clear() throws IOException {
        File workspace = getFile().getParentFile();
        File lockFile = new File(workspace, "flint.lock");
        if ( !lockFile.exists() &&
             !lockFile.createNewFile() ) {
            Logger.getRootLogger().fatal("failed to create flint.lock");
            System.exit(1);
        }
        lockFile.deleteOnExit();
        try (FileOutputStream lockStream = new FileOutputStream(lockFile);
             FileChannel lockChannel = lockStream.getChannel();
             FileLock lock = lockChannel.lock()) {
            for (File child : workspace.listFiles()) {
                if (getFile().equals(child) || lockFile.equals(child))
                    continue;
                if (child.isDirectory()) {
                    File instanceLockFile = new File(child, "flint.lock");
                    if (!instanceLockFile.exists())
                        continue;
                    try (FileOutputStream instanceStream = new FileOutputStream(instanceLockFile);
                         FileChannel instanceChannel = instanceStream.getChannel();
                         FileLock instanceLock = instanceChannel.lock()) {
                        deleteAllBut(child, instanceLockFile);
                    }
                    if (!instanceLockFile.delete()) {
                        Logger.getRootLogger().error("failed to delete " + instanceLockFile);
                    }
                }
                if (!child.delete()) {
                    Logger.getRootLogger().error("failed to delete " + child);
                }
            }
        }
    }

    private static void deleteAllBut(File dir, File file) {
        assert dir.isDirectory();
        for (File child : dir.listFiles()) {
            if (file.equals(child))
                continue;
            recursiveDelete(child);
        }
    }

    private static File getFile() {
        return new File(System.getProperty("user.home"), WORKSPACE_NAME);
    }

    public static File getLockFile() {
        return new File(getFile(), LOCK_FILE_NAME);
    }

    public static File createTempFile(String prefix, String suffix) throws IOException {
        // Prefix must be at least three characters long
        // See
        // http://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#createTempFile(java.nio.file.Path,%20java.lang.String,%20java.lang.String,%20java.nio.file.attribute.FileAttribute...)
        // and
        // http://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String,%20java.io.File)
        while (prefix.length() < 3) {
            prefix += "_";
        }
        Path path = Files.createTempFile(getFile().toPath(), prefix, suffix);
        return path.toFile();
    }

    public static File createTempDirectory(String prefix) throws IOException {
        Path path = Files.createTempDirectory(getFile().toPath(), prefix);
        return path.toFile();
    }

    public static void publishFile(File source, File target) throws IOException {
        Files.copy(source.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING);
    }

    public static void recursiveDelete (File target) {
        if (target  == null)
            return;

        if (target.isDirectory()) {
            for (File f : target.listFiles())
                recursiveDelete (f);
        }

        if (!target.delete()) {
            Logger.getRootLogger().error("failed to delete " + target);
        }
    }

    private static class LockThread extends Thread {

        private final File mFile;

        public LockThread(File file) {
            mFile = file;
            setDaemon(true);
        }

        @Override
        public void run() {
            for (;;) {
                try {
                    FileOutputStream fos = new FileOutputStream(mFile);
                    FileChannel fc = fos.getChannel();
                    try (FileLock lock = fc.lock()) {
                        assert lock.isValid();
                        assert !lock.isShared();
                        Thread.sleep(Long.MAX_VALUE);
                    }
                } catch (InterruptedException | IOException e) {
                    Logger.getRootLogger().fatal("unlocked " + mFile.toString() + ": " + e.getMessage());
                }
            }
        }
    }

    private static class ShutdownHook extends Thread {

        private final File mDir;
        private final File mLockFile;

        public ShutdownHook(File dir, File lockFile) {
            mDir = dir;
            mLockFile = lockFile;
        }

        @Override
        public void run() {
            deleteAllBut(mDir, mLockFile);
        }
    }
}
