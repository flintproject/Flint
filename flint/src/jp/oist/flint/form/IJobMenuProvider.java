/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.dao.DaoException;
import java.io.IOException;
import java.sql.SQLException;

public interface IJobMenuProvider {

	JobMenu getJobMenu(int index) throws DaoException, IOException, SQLException;

}
