import siteExpenseDao from '../dao/siteExpense.dao';
import siteDao from '../dao/site.dao';
import {
  createSiteExpenseBody,
  updateSiteExpenseBody,
} from '../interfaces/siteExpense.Interface';

/**
 * Method to Create a New siteExpense
 * @param body
 * @returns
 */
const createSiteExpense = async (body: createSiteExpenseBody) => {
  try {
    const { site_id, description, amount, date, document_url, created_by } =
      body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getById(site_id);
      if (!siteExist) {
        result = { success: false, message: 'site_id does not exist' };
        return result;
      }
    }

    const siteExpenseDetails = await siteExpenseDao.add(
      site_id,
      description,
      amount,
      date,
      document_url,
      created_by
    );
    result = { success: true, data: siteExpenseDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in siteExpense service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing siteExpense
 * @param body
 * @returns
 */
const updateSiteExpense = async (body: updateSiteExpenseBody) => {
  try {
    const {
      site_id,
      description,
      amount,
      date,
      document_url,
      updated_by,
      site_expense_id,
    } = body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getById(site_id);
      if (!siteExist) {
        result = { success: false, message: 'site_id does not exist' };
        return result;
      }
    }
    const siteExpenseExist = await siteExpenseDao.getById(site_expense_id);

    if (siteExpenseExist) {
      const siteExpenseDetails = await siteExpenseDao.edit(
        site_id,
        description,
        amount,
        date,
        document_url,
        updated_by,
        site_expense_id
      );
      result = { success: true, data: siteExpenseDetails };
      return result;
    } else {
      result = { success: false, message: 'site_expense_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in siteExpense service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get siteExpense By siteExpenseId
 * @param siteExpenseId
 * @returns
 */
const getById = async (siteExpenseId: number) => {
  try {
    let result = null;
    const siteExpenseData = await siteExpenseDao.getById(siteExpenseId);
    if (siteExpenseData) {
      result = { success: true, data: siteExpenseData };
      return result;
    } else {
      result = { success: false, message: 'site_expense_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById siteExpense service : ', error);
    throw error;
  }
};

/**
 * Method to Get All siteExpense's
 * @returns
 */
const getAllSiteExpense = async () => {
  try {
    const result = await siteExpenseDao.getAll();
    const siteExpenseData = { success: true, data: result };
    return siteExpenseData;
  } catch (error) {
    console.log(
      'Error occurred in getAllSiteExpense siteExpense service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete siteExpense
 * @param siteExpenseId
 */
const deleteSiteExpense = async (siteExpenseId: number) => {
  try {
    const siteExpenseExist = await siteExpenseDao.getById(siteExpenseId);
    if (!siteExpenseExist) {
      const result = {
        success: false,
        message: 'site_expense_id does not exist',
      };
      return result;
    }
    const data = await siteExpenseDao.deleteSiteExpense(siteExpenseId);
    if (data) {
      const result = {
        success: true,
        message: 'siteExpense Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this siteExpense',
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteSiteExpense siteExpense service : ',
      error
    );
    throw error;
  }
};

export {
  createSiteExpense,
  updateSiteExpense,
  getAllSiteExpense,
  getById,
  deleteSiteExpense,
};
