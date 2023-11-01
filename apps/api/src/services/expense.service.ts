import expenseDao from '../dao/expense.dao';
import siteDao from '../dao/siteContractor.dao';
import projectDao from '../dao/project.dao';
import { expenseBody } from '../interfaces/expense.interface';
import prisma from '../utils/prisma';
import { processFileDeleteInS3 } from '../utils/fileUpload';
import userDao from '../dao/user.dao';
import expenseDetailsDao from '../dao/expenseDetails.dao';

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
      status,
      total_amount,
      bill_date,
      user_id,
      expense_type,
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

    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        return {
          message: 'user_id id does not exist',
          status: false,
          data: null,
        };
      }
    }

    result = await prisma
      .$transaction(async (prisma) => {
        /*  let expenseExist = null; */
        let expenseDetails = null;
        /*   if (project_id && site_id) {
          expenseExist = await expenseDao.getByProjectIdAndSiteId(
            project_id,
            site_id,
            prisma
          );
        }
        if (!expenseExist) { */
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
          bill_date,
          bill_details,
          created_by,
          status,
          total_amount,
          expense_details,
          user_id,
          expense_type,
          prisma
        );
        /* } else {
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
        } */
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
      bill_date,
      updated_by,
      status,
      expense_id,
      expense_details,
      total_amount,
      bill_details,
      user_id,
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

    if (user_id) {
      const userExist = await userDao.getById(user_id);
      if (!userExist) {
        return {
          message: 'user_id id does not exist',
          status: false,
          data: null,
        };
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

    const updatedBillDetails = [] as any;
    let billDetails = [] as any;
    if (bill_details) {
      billDetails = bill_details;
      for (const doc of billDetails) {
        const { is_delete, path } = doc;

        if (is_delete === 'Y') {
          const deleteDocInS3Body = {
            path,
          };
          await processFileDeleteInS3(deleteDocInS3Body);
        } else {
          updatedBillDetails.push(doc);
        }
      }
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
          bill_date,
          updatedBillDetails,
          updated_by,
          status,
          expense_id,
          total_amount,
          expense_details,
          user_id,
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

    const isUpdateAllDetails = expenseData?.expense_details?.every(
      (expenseDetails: { status: string }) =>
        expenseDetails.status !== 'Pending'
    );

    if (expenseData) {
      result = {
        message: 'success',
        status: true,
        data: { ...expenseData, isEnableComplete: isUpdateAllDetails },
      };
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
      const expenseDetailsData = expenseExist?.expense_details;
      for (const expenseDetails of expenseDetailsData) {
        const expense_details_id = expenseDetails.expense_details_id;
        await expenseDetailsDao.deleteExpenseDetails(expense_details_id);
      }
    }

    const data = await expenseDao.deleteExpense(expenseId);
    if (data) {
      const result = {
        message: 'Expense Data Deleted Successfully',
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
    const project_id = body.project_id;
    const site_id = body.site_id;
    const user_id = body.user_id;
    const expense_status = body.expense_status;
    const employee_name = body.employee_name;
    const is_draft = body.is_draft ? body.is_draft : 'Y';

    const filterObj: any = {};

    if (status) {
      filterObj.filterExpense = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (project_id) {
      filterObj.filterExpense = filterObj.filterExpense || {};
      filterObj.filterExpense.AND = filterObj.filterExpense.AND || [];

      filterObj.filterExpense.AND.push({
        project_id: project_id,
      });
    }

    if (site_id) {
      filterObj.filterExpense = filterObj.filterExpense || {};
      filterObj.filterExpense.AND = filterObj.filterExpense.AND || [];

      filterObj.filterExpense.AND.push({
        site_id: site_id,
      });
    }

    if (expense_status === 'All') {
      if (is_draft === 'Y') {
        filterObj.filterExpense = filterObj.filterExpense || {};
        filterObj.filterExpense.AND = filterObj.filterExpense.AND || [];
      } else {
        filterObj.filterExpense = filterObj.filterExpense || {};
        filterObj.filterExpense.AND = filterObj.filterExpense.AND || [];

        filterObj.filterExpense.AND.push({
          NOT: {
            status: 'Draft',
          },
        });
      }
    } else {
      filterObj.filterExpense = filterObj.filterExpense || {};
      filterObj.filterExpense.AND = filterObj.filterExpense.AND || [];

      filterObj.filterExpense.AND.push({
        status: expense_status,
      });
    }

    if (user_id) {
      filterObj.filterExpense = filterObj.filterExpense || {};
      filterObj.filterExpense.AND = filterObj.filterExpense.AND || [];

      filterObj.filterExpense.AND.push({
        project_data: {
          user_id: user_id,
        },
      });
    }

    if (employee_name) {
      filterObj.filterExpense = filterObj.filterExpense || {};
      filterObj.filterExpense.AND = filterObj.filterExpense.AND || [];

      filterObj.filterExpense.AND.push({
        employee_name: employee_name,
      });
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
      filterObj,
      project_id,
      site_id
    );

    const count = result.data?.count;
    const data = result.data?.data;
    const expenseStatistics = result?.expense_statistics;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempExpenseData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      expense_statistics: expenseStatistics,
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
    const expenseDetailsData = await expenseDao.getExpenseDetailsByExpenseId(
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

/**
 * Method to Update an Existing expense's Status
 * @param body
 * @returns
 */

const updateStatus = async (body: expenseBody) => {
  try {
    const { status, comments, progressed_by, updated_by, expense_id } = body;

    const expenseExist = await expenseDao.getById(expense_id);
    if (!expenseExist) {
      return {
        message: 'expense_id does not exist',
        status: false,
        data: null,
      };
    }

    if (progressed_by) {
      const progressedByUserExist = await userDao.getById(progressed_by);
      if (!progressedByUserExist) {
        return {
          message: 'progressed_by id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const result = await prisma
      .$transaction(async (prisma) => {
        const expenseData = await expenseDao.updateStatus(
          status,
          comments,
          progressed_by,
          updated_by,
          expense_id,
          prisma
        );

        const expenseDetailsByExpenseId =
          await expenseDetailsDao.getByExpenseId(expense_id, prisma);

        // const expenseDetailsUpdatedData = [];
        // for (const expenseDetails of expenseDetailsByExpenseId) {
        //   const expense_details_id = expenseDetails?.expense_details_id;

        //   const expenseDetailsData = await expenseDetailsDao.updateStatus(
        //     status,
        //     comments,
        //     progressed_by,
        //     updated_by,
        //     expense_details_id,
        //     prisma
        //   );

        //   expenseDetailsUpdatedData.push(expenseDetailsData);
        // }

        const expenseDataWithDetails = {
          expense: expenseData,
          expense_details: expenseDetailsByExpenseId,
        };

        return {
          message: 'success',
          status: true,
          data: expenseDataWithDetails,
        };
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
    console.log(
      'Error occurred in expenseDetails service updateStatus: ',
      error
    );
    throw error;
  }
};

/**
 * Method to get expense By expenseCode
 * @param expenseCode
 * @returns
 */
const getByExpenseCode = async (expenseCode: string) => {
  try {
    let result = null;
    const expenseData = await expenseDao.getByExpenseCode(expenseCode);
    if (expenseData) {
      result = {
        message: 'success',
        status: true,
        data: expenseData,
      };
      return result;
    } else {
      result = {
        message: 'No relevant data found for the provided expense_code',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByExpenseCode expense service : ', error);
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
  updateStatus,
  getByExpenseCode,
};
