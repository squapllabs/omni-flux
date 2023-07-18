import prisma from '../utils/prisma';

const add = async (
  site_id: number,
  description: string,
  amount: number,
  date: Date,
  document_url: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_date = date ? new Date(date) : null;

    const siteExpense = await transaction.site_expense.create({
      data: {
        site_id,
        description,
        amount,
        date: formatted_date,
        document_url,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpenseDao add', error);
    throw error;
  }
};

const edit = async (
  site_id: number,
  description: string,
  amount: number,
  date: Date,
  document_url: string,
  updated_by: bigint,
  site_expense_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_date = date ? new Date(date) : null;
    const siteExpense = await transaction.site_expense.update({
      where: {
        site_expense_id: site_expense_id,
      },
      data: {
        site_id,
        description,
        amount,
        date: formatted_date,
        document_url,
        updated_by,
        updated_date: currentDate,
      },
    });
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpenseDao edit', error);
    throw error;
  }
};

const getById = async (siteExpenseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteExpense = await transaction.site_expense.findUnique({
      where: {
        site_expense_id: Number(siteExpenseId),
      },
    });
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpense getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteExpense = await transaction.site_expense.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpense getAll dao', error);
    throw error;
  }
};

const deleteSiteExpense = async (
  siteExpenseId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteExpense = await transaction.site_expense.delete({
      where: {
        site_expense_id: Number(siteExpenseId),
      },
    });
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpense deleteSiteExpense dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteSiteExpense,
};
