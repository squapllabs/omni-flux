import prisma from '../utils/prisma';
import { expenseDetailsBody } from '../interfaces/expense.interface';
import common from './common/utils.dao';
import db from '../utils/db';

const add = async (
  site_id: number,
  project_id: number,
  employee_name: string,
  employee_id: string,
  employee_phone: string,
  purpose: string,
  department: string,
  designation: string,
  start_date: Date,
  end_date: Date,
  bill_details: JSON,
  created_by: number,
  status: string,
  expense_details: Array<expenseDetailsBody>,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;

    const expenseCodeGeneratorQuery = `select
	concat('EXP', DATE_PART('year', CURRENT_DATE), '00', nextval('expence_code_sequence')::text) as expence_code_sequence`;

    const expenseCode = await common.customQueryExecutor(
      expenseCodeGeneratorQuery
    );

    const expense = await transaction.expense.create({
      data: {
        expense_code: expenseCode[0].expence_code_sequence,
        site_id,
        project_id,
        employee_name,
        employee_id,
        employee_phone,
        purpose,
        department,
        designation,
        bill_details: bill_details ? bill_details : [],
        status,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        is_delete: is_delete,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });

    const newExpenseId = expense.expense_id;
    const expenseDetailsData = [];

    for (const expenseDetail of expense_details) {
      const expense_data_id = expenseDetail.expense_data_id;
      const total = expenseDetail.total;
      const bill_details = expenseDetail.bill_details;
      const is_delete = expenseDetail.is_delete;
      const status = expenseDetail.status;
      const comments = expenseDetail.comments;
      const progressed_date = expenseDetail.progressed_date;
      const progressed_by = expenseDetail.progressed_by;
      const bill_number = expenseDetail.bill_number;

      if (is_delete === false) {
        const newExpenseDetail = await transaction.expense_details.create({
          data: {
            expense_id: newExpenseId,
            expense_data_id,
            total,
            bill_details: bill_details ? bill_details : [],
            created_by,
            created_date: currentDate,
            updated_date: currentDate,
            is_delete: false,
            status,
            comments,
            progressed_date,
            progressed_by,
            bill_number,
          },
        });

        expenseDetailsData.push(newExpenseDetail);
      }
    }

    const result = {
      expense: expense,
      expense_details: expenseDetailsData,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in expenseDao add', error);
    throw error;
  }
};

const edit = async (
  site_id: number,
  project_id: number,
  employee_name: string,
  employee_id: string,
  employee_phone: string,
  purpose: string,
  department: string,
  designation: string,
  start_date: Date,
  end_date: Date,
  bill_details: JSON,
  updated_by: number,
  status: string,
  expense_id: number,
  expense_details: Array<expenseDetailsBody>,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const expense = await transaction.expense.update({
      where: {
        expense_id: expense_id,
      },
      data: {
        site_id,
        project_id,
        employee_name,
        employee_id,
        employee_phone,
        purpose,
        department,
        designation,
        bill_details: bill_details ? bill_details : [],
        status,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        updated_by,
        updated_date: currentDate,
      },
    });

    const expenseDetailsData = [];

    for (const expenseDetail of expense_details) {
      const expense_details_id = expenseDetail.expense_details_id;
      const expense_data_id = expenseDetail.expense_data_id;
      const total = expenseDetail.total;
      const bill_details = expenseDetail.bill_details;
      const is_delete = expenseDetail.is_delete;
      const status = expenseDetail.status;
      const comments = expenseDetail.comments;
      const progressed_date = expenseDetail.progressed_date;
      const progressed_by = expenseDetail.progressed_by;
      const bill_number = expenseDetail.bill_number;

      if (expense_details_id) {
        if (is_delete === true) {
          await transaction.expense_details.update({
            where: {
              expense_details_id: Number(expense_details_id),
            },
            data: {
              is_delete: true,
            },
          });
        } else {
          const newExpenseDetail = await transaction.expense_details.update({
            where: { expense_details_id: Number(expense_details_id) },
            data: {
              expense_id: expense_id,
              expense_data_id,
              total,
              bill_details: bill_details ? bill_details : [],
              updated_by,
              updated_date: currentDate,
              status,
              comments,
              progressed_date,
              progressed_by,
              bill_number,
            },
          });

          expenseDetailsData.push(newExpenseDetail);
        }
      } else {
        if (is_delete === false) {
          const newExpenseDetail = await transaction.expense_details.create({
            data: {
              expense_id: expense_id,
              expense_data_id,
              total,
              bill_details: bill_details ? bill_details : [],
              created_by: updated_by,
              created_date: currentDate,
              updated_date: currentDate,
              is_delete: false,
              status,
              comments,
              progressed_date,
              progressed_by,
              bill_number,
            },
          });

          expenseDetailsData.push(newExpenseDetail);
        }
      }
    }
    const result = {
      expense: expense,
      expense_details: expenseDetailsData,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in expenseDao edit', error);
    throw error;
  }
};

const getById = async (expenseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expense = await transaction.expense.findFirst({
      where: {
        expense_id: Number(expenseId),
        is_delete: false,
      },
      include: {
        expense_details: {
          include: {
            progressed_by_data: {
              select: {
                first_name: true,
                last_name: true,
              },
            },
            expense_master_data: true,
          },
        },
        site_data: {
          select: {
            name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
    });
    return expense;
  } catch (error) {
    console.log('Error occurred in expense getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expense = await transaction.expense.findMany({
      where: {
        is_delete: false,
      },
      include: {
        expense_details: {
          include: {
            progressed_by_data: {
              select: {
                first_name: true,
                last_name: true,
              },
            },
            expense_master_data: true,
          },
        },
        site_data: {
          select: {
            name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return expense;
  } catch (error) {
    console.log('Error occurred in expense getAll dao', error);
    throw error;
  }
};

const deleteExpense = async (expenseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expense = await transaction.expense.update({
      where: {
        expense_id: Number(expenseId),
      },
      data: {
        is_delete: true,
      },
    });
    return expense;
  } catch (error) {
    console.log('Error occurred in expense deleteExpense dao', error);
    throw error;
  }
};

const searchExpense = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  project_id: number,
  site_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterExpense;
    const expense = await transaction.expense.findMany({
      where: filter,
      include: {
        expense_details: {
          include: {
            progressed_by_data: {
              select: {
                first_name: true,
                last_name: true,
              },
            },
            expense_master_data: true,
          },
        },
        site_data: {
          select: {
            name: true,
          },
        },
        project_data: {
          include: {
            project_member_association: true,
          },
        },
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });

    const expenseCount = await transaction.expense.count({
      where: filter,
    });
    const expenseData = {
      count: expenseCount,
      data: expense,
    };

    let expenseStatistics = {};
    if (project_id && site_id) {
      const db_transaction = connectionObj !== null ? connectionObj : db;

      const expenseStatisticsQuery = `SELECT
          CAST((SELECT COUNT(*) FROM expense WHERE project_id = ${project_id} AND site_id = ${site_id}) AS INT) AS total_expenses,
          SUM(CASE WHEN e.status = 'Approved' THEN ed.total ELSE 0 END) AS approved_expenses,
          SUM(CASE WHEN e.status = 'Rejected' THEN ed.total ELSE 0 END) AS rejected_expenses,
          SUM(CASE WHEN e.status = 'Pending' THEN ed.total ELSE 0 END) AS pending_expenses
      FROM
          expense e
      LEFT JOIN
          expense_details ed ON ed.expense_id = e.expense_id
      WHERE
          e.project_id =  ${project_id}
          AND e.site_id = ${site_id}`;

      expenseStatistics = await db_transaction.one(expenseStatisticsQuery);
    }
    const expenseDataResult = {
      expense_statistics: expenseStatistics,
      data: expenseData,
    };

    return expenseDataResult;
  } catch (error) {
    console.log('Error occurred in Expense dao : searchExpense ', error);
    throw error;
  }
};

const getByProjectIdAndSiteId = async (
  projectId: number,
  siteId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expense = await transaction.expense.findFirst({
      where: {
        project_id: Number(projectId),
        site_id: Number(siteId),
        is_delete: false,
      },
      include: {
        expense_details: {
          where: {
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
        },
        site_data: {
          select: {
            name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
    });

    return expense;
  } catch (error) {
    console.log('Error occurred in expense getByProjectIdAndSiteId dao', error);
    throw error;
  }
};

const getExpenseDetailsByExpenceId = async (
  expenseId: number,
  status: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expenseDetails = await transaction.expense.findFirst({
      where: {
        expense_id: Number(expenseId),
        is_delete: false,
      },
      include: {
        expense_details: {
          where: {
            status: String(status),
          },
        },
      },
    });

    return expenseDetails;
  } catch (error) {
    console.log('Error occurred in expense getByProjectIdAndSiteId dao', error);
    throw error;
  }
};

const updateStatus = async (
  status: string,
  comments: string,
  progressed_by: number,
  updated_by: number,
  expense_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expense = await transaction.expense.update({
      where: {
        expense_id: Number(expense_id),
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

    return expense;
  } catch (error) {
    console.log('Error occurred in expenseDao updateStatus', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteExpense,
  searchExpense,
  getByProjectIdAndSiteId,
  getExpenseDetailsByExpenceId,
  updateStatus,
};
