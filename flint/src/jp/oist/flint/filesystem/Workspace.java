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
    private static final String LOG_FILE_NAME = "flint.log";

    private final static String WORKSPACE_NAME;

    private static File mLockFile;
    private static FileLock mFileLock; 

    static {
        WORKSPACE_NAME = new File(".flint", UUID.randomUUID().toString()).getPath();

        try {
            File dir = getFile();
            dir.mkdirs();
            mLockFile = new File(dir, LOCK_FILE_NAME);
            if (!mLockFile.exists()) mLockFile.createNewFile();
            mLockFile.deleteOnExit();
            FileOutputStream lockStream = new FileOutputStream(mLockFile);
            FileChannel lockChannel = lockStream.getChannel();
            mFileLock = lockChannel.tryLock();

            clear();
        } catch (IOException ioe) {
            // ignored.
        } finally {
            if (mFileLock == null) {
                Logger.getRootLogger().fatal("could not get lock: " + mLockFile.toString());
                System.exit(1);
            }
        }

        Runtime.getRuntime().addShutdownHook(new ShutdownHook());
    }

    private static void clear() throws IOException {
        File lockFile = null;
        FileLock lock = null;
        FileChannel lockChannel = null;
        try {
            File flintWorkspace = getFile().getParentFile();

            lockFile = new File(flintWorkspace, "flint.lock");
            if (!lockFile.exists())
                lockFile.createNewFile();

            FileOutputStream lockStream = new FileOutputStream(lockFile);
            lockChannel = lockStream.getChannel();

            lock = lockChannel.tryLock();
            if (lock == null)
                throw new IOException("could not create the lock file");

            for (File child : flintWorkspace.listFiles()) {
                if (getFile().equals(child) || lockFile.equals(child))
                        continue;

                FileChannel instanceChannel = null;
                try {
                    File instanceLockFile = new File(child, "flint.lock");

                    if (instanceLockFile.exists()) {
                        FileLock instanceLock = null;
                        try {
                            FileOutputStream instanceStream 
                                    = new FileOutputStream(instanceLockFile);
                            instanceChannel = instanceStream.getChannel();
                            instanceLock = instanceChannel.tryLock();
                            if (instanceLock == null)
                                continue;
                        } finally {
                            if (instanceLock != null)
                                instanceLock.release();

                            if (instanceChannel != null)
                                instanceChannel.close();
                        }
                    }

                    recursiveDelete(child);
                } catch (IOException ex) {
                    Logger.getRootLogger().error(ex.getMessage());
                } finally {
                }
            }
        } catch (IOException ex) {
            Logger.getRootLogger().error("could not create the lock file.");
            return;
        } finally {
            if (lock != null)
                lock.release();
            if (lockChannel != null)
                lockChannel.close();
            if (lockFile != null && lockFile.exists())
                lockFile.delete();
        }
    }

    private static File getFile() throws IOException {
        return new File(getPath());
    }

    // TODO: This method name becomes a misnomer, since Java 7 introduces java.nio.file.Path.
    public static String getPath() throws IOException {
        return System.getProperty("user.home") + File.separator + WORKSPACE_NAME;
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

        target.delete();
    }

    private static class ShutdownHook extends Thread {

        @Override
        public void run() {
            if (mFileLock != null) {
                try (FileChannel channel = mFileLock.channel()) {
                    Workspace.recursiveDelete(Workspace.getFile());
                    mFileLock.release();
                } catch (IOException ioe) {
                    // ignored
                }
            }
        }
    }
}
