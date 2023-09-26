import prisma from '../utils/prisma';

const updateStatus = async (
  status: string,
  comments: string,
  progressed_by: number,
  updated_by: number,
  expense_details_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expenseDetails = await transaction.expense_details.update({
      where: {
        expense_details_id: expense_details_id,
      },
      data: {
        status,
        comments,
        progressed_date: currentDate,
        progressed_by,
        updated_by,
        updated_date: currentDate,
      },
    });

    return expenseDetails;
  } catch (error) {
    console.log('Error occurred in expenseDetailsDao updateStatus', error);
    throw error;
  }
};

const getById = async (expense_details_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expenseDetails = await transaction.expense_details.findFirst({
      where: {
        expense_details_id: Number(expense_details_id),
        is_delete: false,
      },
      include: {
        progressed_by_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        expense_master_data: true,
      },
    });
    return expenseDetails;
  } catch (error) {
    console.log('Error occurred in expenseDetailsDao getById', error);
    throw error;
  }
};

const getByExpenseId = async (expense_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expenseDetails = await transaction.expense_details.findMany({
      where: {
        expense_id: expense_id,
        is_delete: false,
      },
    });
    return expenseDetails;
  } catch (error) {
    console.log('Error occurred in expenseDetailsDao getByExpenseId', error);
    throw error;
  }
};

export default {
  updateStatus,
  getById,
  getByExpenseId,
};
