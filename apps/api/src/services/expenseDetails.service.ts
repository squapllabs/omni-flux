import expenseDao from '../dao/expense.dao';
import expenseDetailsDao from '../dao/expenseDetails.dao';
import userDao from '../dao/user.dao';
import { expenseDetailsBody } from '../interfaces/expense.interface';
import prisma from '../utils/prisma';

/**
 * Method to Update an Existing expenseDetails Status
 * @param body
 * @returns
 */

const updateStatus = async (body: expenseDetailsBody) => {
  try {
    const { status, comments, progressed_by, updated_by, expense_details_id } =
      body;

    const expenseDetailsExist = await expenseDetailsDao.getById(
      expense_details_id
    );
    if (!expenseDetailsExist) {
      return {
        message: 'expense_details_id does not exist',
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
        const expenseDetailsData = await expenseDetailsDao.updateStatus(
          status,
          comments,
          progressed_by,
          updated_by,
          expense_details_id,
          prisma
        );

        const expense_id = expenseDetailsExist?.expense_id;
        // const expenseDetailsByExpenseId =
        //   await expenseDetailsDao.getByExpenseId(expense_id, prisma);

        let dbExpense = await expenseDao.getByIdWithOutChild(expense_id, prisma);

        if (dbExpense.status === 'Pending') {
          dbExpense = await expenseDao.updateStatus(
            'InProgress',
            'Some of the respective child expense details has been rejected',
            progressed_by,
            updated_by,
            expense_id,
            prisma
          );
        }

        /**
                let expenseStatusUpdate = null;
                console.log("check Status Data::0", expenseDetailsByExpenseId)
                const allApproved = expenseDetailsByExpenseId.every(
                  (expenseDetails: { status: string }) =>
                    expenseDetails.status === 'Approved'
                );
                console.log("check Status Data::2", allApproved)
                // Check if at least one expenseDetails has 'Rejected' status 
                const hasRejected = expenseDetailsByExpenseId.some(
                  (expenseDetails: { status: string }) =>
                    expenseDetails.status === 'Rejected'
                );
        
                if (allApproved) {
                  //  Trigger the approval flow 
                  expenseStatusUpdate = await expenseDao.updateStatus(
                    status,
                    comments,
                    progressed_by,
                    updated_by,
                    expense_id,
                    prisma
                  );
                  console.log(
                    'All expense details are Approved. So Expense is Approved::', expenseStatusUpdate
                  );
                } else if (hasRejected) {
                  // Trigger the separate flow for 'Rejected' 
                  expenseStatusUpdate = await expenseDao.updateStatus(
                    'Rejected',
                    'Some of the respective child expense details has been rejected',
                    progressed_by,
                    updated_by,
                    expense_id,
                    prisma
                  );
                  console.log(
                    'Some of the expense details got Rejected.So Expense is Rejected'
                  );
                }
                **/

        const expenseDetailsWithExpenseData = {
          expense_details: expenseDetailsData,
          expense: dbExpense,
        };

        return {
          message: 'success',
          status: true,
          data: expenseDetailsWithExpenseData,
        };
      })
      .then((data) => {
        console.log('Successfully Expense Details Data Returned ', data);
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
 * Method to get expenseDetails By Id
 * @param expense_details_id
 * @returns
 */

const getById = async (expense_details_id: number) => {
  try {
    const expenseDetailsExist = await expenseDetailsDao.getById(
      expense_details_id
    );
    if (expenseDetailsExist) {
      return {
        message: 'success',
        status: true,
        data: expenseDetailsExist,
      };
    } else {
      return {
        message: 'expense_details_id does not exist',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log('Error occurred in expenseDetails service getById: ', error);
    throw error;
  }
};

/**
 * Method to search Expense Details- Pagination API
 * @returns
 */
const searchExpenseDetails = async (body) => {
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
    const expense_id = body.expense_id;
    const expense_status = body.expense_status;

    const filterObj: any = {};

    if (status) {
      filterObj.filterExpenseDetails = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (expense_id) {
      filterObj.filterExpenseDetails = filterObj.filterExpenseDetails || {};
      filterObj.filterExpenseDetails.AND =
        filterObj.filterExpenseDetails.AND || [];

      filterObj.filterExpenseDetails.AND.push({
        expense_id: expense_id,
      });
    }

    if (expense_status) {
      filterObj.filterExpenseDetails = filterObj.filterExpenseDetails || {};
      filterObj.filterExpenseDetails.AND =
        filterObj.filterExpenseDetails.AND || [];

      filterObj.filterExpenseDetails.AND.push({
        status: expense_status,
      });
    }

    if (global_search) {
      filterObj.filterExpenseDetails = filterObj.filterExpenseDetails || {};
      filterObj.filterExpenseDetails.OR =
        filterObj.filterExpenseDetails.OR || [];

      filterObj.filterExpenseDetails.OR.push(
        {
          progressed_by_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          progressed_by_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          expense_master_data: {
            master_data_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          description: {
            contains: global_search,
            mode: 'insensitive',
          },
        }
      );
    }

    const result = await expenseDetailsDao.searchExpenseDetails(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempExpenseDetailsData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempExpenseDetailsData;
  } catch (error) {
    console.log(
      'Error occurred in searchExpenseDetails expense details service : ',
      error
    );
    throw error;
  }
};

export { updateStatus, getById, searchExpenseDetails };
