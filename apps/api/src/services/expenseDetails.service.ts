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
        const expenseDetailsByExpenseId =
          await expenseDetailsDao.getByExpenseId(expense_id, prisma);

        let expense_details_status_same = false;
        let expense_details_status_different = false;

        for (const expenseDetails of expenseDetailsByExpenseId) {
          const expense_details_existing_status = expenseDetails?.status;
          if (expense_details_existing_status === status) {
            expense_details_status_same = true;
          } else {
            expense_details_status_different = true;
          }
        }

        let expenseStatusUpdate = null;

        if (
          expense_details_status_same === true &&
          expense_details_status_different === false
        ) {
          expenseStatusUpdate = await expenseDao.updateStatus(
            status,
            comments,
            progressed_by,
            updated_by,
            expense_id,
            prisma
          );
        } else if (expense_details_status_different === true) {
          expenseStatusUpdate = await expenseDao.updateStatus(
            'Rejected',
            'Some of the respective child expense details has been rejected',
            progressed_by,
            updated_by,
            expense_id,
            prisma
          );
        }

        const expenseDetailsWithExpenseData = {
          expense_details: expenseDetailsData,
          expense: expenseStatusUpdate,
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

export { updateStatus, getById };
