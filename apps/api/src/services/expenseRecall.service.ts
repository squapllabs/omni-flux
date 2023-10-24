import expenseDao from '../dao/expense.dao';
import expenseDetailsDao from '../dao/expenseDetails.dao';
import expenseRecallDao from '../dao/expenseRecall.dao';
import projectDao from '../dao/project.dao';
import siteContractorDao from '../dao/siteContractor.dao';
import userDao from '../dao/user.dao';
import { expenseRecallBody } from '../interfaces/expenseRecall.interface';
import prisma from '../utils/prisma';
/**
 * Method to Create a New expenseRecall
 * @param body
 * @returns
 */
const createExpenseRecall = async (body: expenseRecallBody) => {
  try {
    const {
      project_id,
      site_id,
      expense_id,
      expense_details_id,
      recall_creator_id,
      recall_date,
      reason,
      created_by,
    } = body;

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (site_id) {
      const siteExist = await siteContractorDao.getBySiteId(site_id);
      if (!siteExist) {
        return {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (expense_id) {
      const expenseExist = await expenseDao.getById(expense_id);
      if (!expenseExist) {
        return {
          message: 'expense_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (expense_details_id) {
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
    }

    if (recall_creator_id) {
      const userExist = await userDao.getById(recall_creator_id);
      if (!userExist) {
        return {
          message: 'recall_creator_id does not exist',
          status: false,
          data: null,
        };
      }
    }
    const result = await prisma
      .$transaction(async (prisma) => {
        const expenseRecallDetails = await expenseRecallDao.add(
          project_id,
          site_id,
          expense_id,
          expense_details_id,
          recall_creator_id,
          recall_date,
          reason,
          created_by,
          prisma
        );
        return expenseRecallDetails;
      })
      .then((data) => {
        console.log('Successfully Expense Recall Data Returned ', data);
        const expenseRecallDetailsData = {
          message: 'success',
          status: true,
          data: data,
        };
        return expenseRecallDetailsData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in expenseRecall service Add: ', error);
    throw error;
  }
};

export { createExpenseRecall };
