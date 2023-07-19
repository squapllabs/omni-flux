import prisma from '../utils/prisma';

const add = async (
  project_id: number | null,
  description: string | null,
  amount: number | null,
  date: Date | null,
  document_url: string | null,
  created_by: bigint | null,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_date = date ? new Date(date) : null;

    const projectExpense = await transaction.project_expense.create({
      data: {
        project_id,
        description,
        amount,
        date: formatted_date,
        document_url,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return projectExpense;
  } catch (error) {
    console.log('Error occurred in projectExpenseDao add', error);
    throw error;
  }
};

const edit = async (
  project_id: number,
  description: string,
  amount: number,
  date: Date,
  document_url: string,
  updated_by: bigint,
  project_expense_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_date = date ? new Date(date) : null;
    const projectExpense = await transaction.project_expense.update({
      where: {
        project_expense_id: project_expense_id,
      },
      data: {
        project_id,
        description,
        amount,
        date: formatted_date,
        document_url,
        updated_by,
        updated_date: currentDate,
      },
    });
    return projectExpense;
  } catch (error) {
    console.log('Error occurred in projectExpenseDao edit', error);
    throw error;
  }
};

const getById = async (projectExpenseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectExpense = await transaction.project_expense.findUnique({
      where: {
        project_expense_id: Number(projectExpenseId),
      },
    });
    return projectExpense;
  } catch (error) {
    console.log('Error occurred in projectExpense getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectExpense = await transaction.project_expense.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return projectExpense;
  } catch (error) {
    console.log('Error occurred in projectExpense getAll dao', error);
    throw error;
  }
};

const deleteProjectExpense = async (
  projectExpenseId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectExpense = await transaction.project_expense.delete({
      where: {
        project_expense_id: Number(projectExpenseId),
      },
    });
    return projectExpense;
  } catch (error) {
    console.log(
      'Error occurred in projectExpense deleteProjectExpense dao',
      error
    );
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteProjectExpense,
};
