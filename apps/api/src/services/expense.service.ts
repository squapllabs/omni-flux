import expenseDao from '../dao/expense.dao';
import siteDao from '../dao/siteContractor.dao';
import projectDao from '../dao/project.dao';
import { expenseBody } from '../interfaces/expense.interface';
import prisma from '../utils/prisma';

/**
 * Method to Create a New expense
 * @param body
 * @returns
 */
const createExpense = async (body: expenseBody) => {
  try {
    const {
      site_id,
      project_id,
      employee_name,
      employee_id,
      employee_phone,
      purpose,
      department,
      designation,
      start_date,
      end_date,
      created_by,
      expense_details,
      bill_details,
    } = body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getBySiteId(site_id);
      if (!siteExist) {
        result = {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }
    result = await prisma
      .$transaction(async (prisma) => {
        let expenseExist = null;
        let expenseDetails = null;
        if (project_id && site_id) {
          expenseExist = await expenseDao.getByProjectIdAndSiteId(
            project_id,
            site_id,
            prisma
          );
        }
        if (!expenseExist) {
          expenseDetails = await expenseDao.add(
            site_id,
            project_id,
            employee_name,
            employee_id,
            employee_phone,
            purpose,
            department,
            designation,
            start_date,
            end_date,
            bill_details,
            created_by,
            expense_details,
            prisma
          );
        } else {
          expenseDetails = await expenseDao.edit(
            site_id,
            project_id,
            employee_name,
            employee_id,
            employee_phone,
            purpose,
            department,
            designation,
            start_date,
            end_date,
            bill_details,
            created_by,
            expenseExist?.expense_id,
            expense_details,
            prisma
          );
        }
        result = { message: 'success', status: true, data: expenseDetails };
        return result;
      })
      .then((data) => {
        console.log('Successfully Expense Returned ');
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in expense service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing expense
 * @param body
 * @returns
 */
const updateExpense = async (body: expenseBody) => {
  try {
    const {
      site_id,
      project_id,
      employee_name,
      employee_id,
      employee_phone,
      purpose,
      department,
      designation,
      start_date,
      end_date,
      updated_by,
      expense_id,
      expense_details,
      bill_details,
    } = body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getBySiteId(site_id);
      if (!siteExist) {
        result = {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const expenseExist = await expenseDao.getById(expense_id);
    if (!expenseExist) {
      return {
        message: 'expense_id does not exist',
        status: false,
        data: null,
      };
    }

    result = await prisma
      .$transaction(async (prisma) => {
        const expenseDetails = await expenseDao.edit(
          site_id,
          project_id,
          employee_name,
          employee_id,
          employee_phone,
          purpose,
          department,
          designation,
          start_date,
          end_date,
          bill_details,
          updated_by,
          expense_id,
          expense_details,
          prisma
        );
        result = { message: 'success', status: true, data: expenseDetails };
        return result;
      })
      .then((data) => {
        console.log('Successfully Expense Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in expense service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get expense By expenseId
 * @param expenseId
 * @returns
 */
const getById = async (expenseId: number) => {
  try {
    let result = null;
    const expenseData = await expenseDao.getById(expenseId);
    if (expenseData) {
      result = { message: 'success', status: true, data: expenseData };
      return result;
    } else {
      result = {
        message: 'expense_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById expense service : ', error);
    throw error;
  }
};

/**
 * Method to Get All expense's
 * @returns
 */
const getAllExpense = async () => {
  try {
    const result = await expenseDao.getAll();
    const expenseData = { message: 'success', status: true, data: result };
    return expenseData;
  } catch (error) {
    console.log('Error occurred in getAllExpense expense service : ', error);
    throw error;
  }
};

/**
 * Method to delete expense
 * @param expenseId
 */
const deleteExpense = async (expenseId: number) => {
  try {
    const expenseExist = await expenseDao.getById(expenseId);
    if (!expenseExist) {
      const result = {
        message: 'expense_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (expenseExist?.expense_details?.length > 0) {
      const result = {
        message:
          'Unable to delete this.The expense_id is mapped on expense_details',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await expenseDao.deleteExpense(expenseId);
    if (data) {
      const result = {
        message: 'expense Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this expense',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteExpense expense service : ', error);
    throw error;
  }
};

/**
 * Method to search Expense - Pagination API
 * @returns
 */
const searchExpense = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterExpense = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (global_search) {
      filterObj.filterExpense = filterObj.filterExpense || {};
      filterObj.filterExpense.OR = filterObj.filterExpense.OR || [];

      filterObj.filterExpense.OR.push(
        {
          employee_name: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          employee_id: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          employee_phone: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          purpose: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          department: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          designation: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          site_data: {
            name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_data: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );

      filterObj.filterExpense.OR.push({
        OR: [
          {
            expense_details: {
              some: {
                progressed_by_data: {
                  first_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                  last_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
          {
            expense_details: {
              some: {
                expense_master_data: {
                  master_data_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        ],
      });
    }

    const result = await expenseDao.searchExpense(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempExpenseData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempExpenseData;
  } catch (error) {
    console.log('Error occurred in searchExpense expense service : ', error);
    throw error;
  }
};

/**
 * Method to get expense By Project Id and Site Id
 * @param project_id
 * @param site_id
 * @returns
 */
const getByProjectIdAndSiteId = async (project_id: number, site_id: number) => {
  try {
    let result = null;
    const expenseData = await expenseDao.getByProjectIdAndSiteId(
      project_id,
      site_id
    );
    if (expenseData) {
      result = { message: 'success', status: true, data: expenseData };
      return result;
    } else {
      result = {
        message:
          'expense does not exist for this project_id and site_id combination',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectIdAndSiteId expense service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get expense By Project Id and Site Id
 * @param project_id
 * @param site_id
 * @returns
 */
const getExpenseDetailsByExpenseId = async (expense_id: number) => {
  try {
    let result = null;
    const status = 'Pending';
    const expenseDetailsData = await expenseDao.getExpenseDetailsByExpenceId(
      expense_id,
      status
    );
    if (expenseDetailsData) {
      result = {
        message: 'success',
        status: true,
        data: expenseDetailsData,
      };
      return result;
    } else {
      result = {
        message: 'expense does not exist for this expense_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectIdAndSiteId expense service : ',
      error
    );
    throw error;
  }
};

export {
  createExpense,
  updateExpense,
  getAllExpense,
  getById,
  deleteExpense,
  searchExpense,
  getByProjectIdAndSiteId,
  getExpenseDetailsByExpenseId,
};
