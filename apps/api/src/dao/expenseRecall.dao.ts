import prisma from '../utils/prisma';

const add = async (
  project_id: number,
  site_id: number,
  expense_id: number,
  expense_details_id: number,
  recall_creator_id: number,
  recall_date: Date,
  reason: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_recall_date = recall_date ? new Date(recall_date) : null;
    const expenseRecall = await transaction.expense_recall.create({
      data: {
        project_id,
        site_id,
        expense_id,
        expense_details_id,
        recall_creator_id,
        recall_date: formatted_recall_date,
        reason,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });

    if (expenseRecall) {
      await transaction.expense_details.update({
        where: {
          expense_details_id: expense_details_id,
        },
        data: {
          is_recalled: true,
          updated_by: created_by,
          updated_date: currentDate,
        },
      });
    }

    return expenseRecall;
  } catch (error) {
    console.log('Error occurred in expenseRecallDao add', error);
    throw error;
  }
};

export default {
  add,
};
