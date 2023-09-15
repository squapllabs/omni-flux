import projectExpenseDao from '../dao/projectExpense.dao';
import projectDao from '../dao/project.dao';
import {
  createProjectExpenseBody,
  updateProjectExpenseBody,
} from '../interfaces/projectExpense.Interface';

/**
 * Method to Create a New ProjectExpense
 * @param body
 * @returns
 */
const createProjectExpense = async (body: createProjectExpenseBody) => {
  try {
    const { project_id, description, amount, date, document_url, created_by } =
      body;
    let result = null;

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = { message: 'project_id does not exist', status: false, data: null };
        return result;
      }
    }

    const projectExpenseDetails = await projectExpenseDao.add(
      project_id,
      description,
      amount,
      date,
      document_url,
      created_by
    );
    result = { message: 'success', status: true, data: projectExpenseDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in projectExpense service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing ProjectExpense
 * @param body
 * @returns
 */
const updateProjectExpense = async (body: updateProjectExpenseBody) => {
  try {
    const {
      project_id,
      description,
      amount,
      date,
      document_url,
      updated_by,
      project_expense_id,
    } = body;
    let result = null;

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = { message: 'project_id does not exist', status: false, data: null };
        return result;
      }
    }
    const projectExpenseExist = await projectExpenseDao.getById(
      project_expense_id
    );

    if (projectExpenseExist) {
      const projectExpenseDetails = await projectExpenseDao.edit(
        project_id,
        description,
        amount,
        date,
        document_url,
        updated_by,
        project_expense_id
      );
      result = { message: 'success', status: true, data: projectExpenseDetails };
      return result;
    } else {
      result = { message: 'project_expense_id does not exist', status: false, data: null };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in projectExpense service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get ProjectExpense By projectExpenseId
 * @param projectExpenseId
 * @returns
 */
const getById = async (projectExpenseId: number) => {
  try {
    let result = null;
    const projectExpenseData = await projectExpenseDao.getById(
      projectExpenseId
    );
    if (projectExpenseData) {
      result = { message: 'success', status: true, data: projectExpenseData };
      return result;
    } else {
      result = { message: 'project_expense_id does not exist', status: false, data: null };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById projectExpense service : ', error);
    throw error;
  }
};

/**
 * Method to Get All ProjectExpense's
 * @returns
 */
const getAllProjectExpense = async () => {
  try {
    const result = await projectExpenseDao.getAll();
    const projectExpenseData = { message: 'success', status: true, data: result };
    return projectExpenseData;
  } catch (error) {
    console.log(
      'Error occurred in getAllProjectExpense projectExpense service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete projectExpense
 * @param projectExpenseId
 */
const deleteProjectExpense = async (projectExpenseId: number) => {
  try {
    const projectExpenseExist = await projectExpenseDao.getById(
      projectExpenseId
    );
    if (!projectExpenseExist) {
      const result = {
        message: 'project_expense_id does not exist',
        status: false,
        data: null
      };
      return result;
    }
    const data = await projectExpenseDao.deleteProjectExpense(projectExpenseId);
    if (data) {
      const result = {
        message: 'ProjectExpense Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this projectExpense',
        status: false,
        data: null
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteProjectExpense projectExpense service : ',
      error
    );
    throw error;
  }
};

export {
  createProjectExpense,
  updateProjectExpense,
  getAllProjectExpense,
  getById,
  deleteProjectExpense,
};
