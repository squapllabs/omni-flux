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
  bill_date: Date,
  bill_details: JSON,
  created_by: number,
  status: string,
  total_amount: number,
  expense_details: Array<expenseDetailsBody>,
  user_id: number,
  expense_type: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const formatted_bill_date = bill_date ? new Date(bill_date) : null;

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
        total_amount,
        bill_details: bill_details ? bill_details : [],
        status,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        bill_date: formatted_bill_date,
        is_delete: is_delete,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        user_id,
        expense_type,
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
      const description = expenseDetail.description;
      const quantity = expenseDetail.quantity;
      const unit_value = expenseDetail.unit_value;
      const bill_type = expenseDetail.bill_type;

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
            description,
            quantity,
            unit_value,
            bill_type,
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
  bill_date: Date,
  bill_details: JSON,
  updated_by: number,
  status: string,
  expense_id: number,
  total_amount: number,
  expense_details: Array<expenseDetailsBody>,
  user_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const formatted_bill_date = bill_date ? new Date(bill_date) : null;

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
        total_amount,
        bill_details: bill_details ? bill_details : [],
        status,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        bill_date: formatted_bill_date,
        updated_by,
        updated_date: currentDate,
        user_id,
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
      const description = expenseDetail.description;
      const quantity = expenseDetail.quantity;
      const unit_value = expenseDetail.unit_value;
      const bill_type = expenseDetail.bill_type;

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
              description,
              quantity,
              unit_value,
              bill_type,
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
              description,
              quantity,
              unit_value,
              bill_type,
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
          orderBy: [{ created_date: 'asc' }],
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
        user_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
      },
    });

    const [total] = await transaction.$queryRaw`SELECT
        SUM(CASE WHEN ed.status = 'Approved' THEN ed.total ELSE 0 END) AS approved_total,
        SUM(CASE WHEN ed.status = 'Rejected' THEN ed.total ELSE 0 END) AS rejected_total,
        SUM(CASE WHEN ed.status = 'Pending' THEN ed.total ELSE 0 END) AS pending_total
    FROM
        expense_details ed
    WHERE
        ed.expense_id = ${Number(expenseId)}`;

    expense.approved_total = total.approved_total;
    expense.rejected_total = total.rejected_total;
    expense.pending_total = total.pending_total;

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
        user_data: {
          select: {
            first_name: true,
            last_name: true,
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
          include: {
            project_member_association: true,
          },
        },
        user_data: {
          select: {
            first_name: true,
            last_name: true,
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

      const expenseStatisticsQuery = `select
      cast((select COUNT(*) from expense where project_id =  ${project_id} and site_id = ${site_id} and is_delete = false) as INT) as total_expenses,
      SUM(case when e.status = 'Completed' then ed.total else 0 end) as completed_expenses,
      SUM(case when e.status = 'InProgress' then ed.total else 0 end) as inprogress_expenses,
      SUM(case when e.status = 'Pending' then ed.total else 0 end) as pending_expenses,
      SUM(case when e.status = 'Draft' then ed.total else 0 end) as draft_expenses
    from
      expense e
    left join
              expense_details ed on
      ed.expense_id = e.expense_id
    where
      e.project_id =  ${project_id}
      and e.site_id = ${site_id}
      and e.is_delete = false`;

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
        user_data: {
          select: {
            first_name: true,
            last_name: true,
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

const getExpenseDetailsByExpenseId = async (
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

const getByIdWithOutChild = async (expenseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expense = await transaction.expense.findFirst({
      where: {
        expense_id: Number(expenseId),
        is_delete: false,
      },
    });
    return expense;
  } catch (error) {
    console.log('Error occurred in expense getByIdWithOutChild dao', error);
    throw error;
  }
};

const getByExpenseCode = async (expenseCode: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const expense = await transaction.expense.findFirst({
      where: {
        expense_code: expenseCode,
        is_delete: false,
        status: 'Completed',
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
          orderBy: [{ created_date: 'asc' }],
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
        user_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
      },
    });

    return expense;
  } catch (error) {
    console.log('Error occurred in expense getByExpenseCode dao', error);
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
  getExpenseDetailsByExpenseId,
  updateStatus,
  getByIdWithOutChild,
  getByExpenseCode,
};
