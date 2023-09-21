import prisma from '../utils/prisma';

const add = async (
  project_id: number,
  site_id: number,
  stock_audit_date: Date,
  item_details: JSON,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_stock_audit_date = stock_audit_date
      ? new Date(stock_audit_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockAudit = await transaction.stock_audit.create({
      data: {
        project_id,
        site_id,
        stock_audit_date: formatted_stock_audit_date,
        item_details,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return stockAudit;
  } catch (error) {
    console.log('Error occurred in stockAuditDao add', error);
    throw error;
  }
};

const edit = async (
  project_id: number,
  site_id: number,
  stock_audit_date: Date,
  item_details: JSON,
  updated_by: number,
  stock_audit_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_stock_audit_date = stock_audit_date
      ? new Date(stock_audit_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockAudit = await transaction.stock_audit.update({
      where: {
        stock_audit_id: stock_audit_id,
      },
      data: {
        project_id,
        site_id,
        stock_audit_date: formatted_stock_audit_date,
        item_details,
        updated_by,
        updated_date: currentDate,
      },
    });
    return stockAudit;
  } catch (error) {
    console.log('Error occurred in stockAuditDao edit', error);
    throw error;
  }
};

const getById = async (stockAuditId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockAudit = await transaction.stock_audit.findFirst({
      where: {
        stock_audit_id: Number(stockAuditId),
        is_delete: false,
      },
      include: { project_data: true, site_data: true },
    });
    return stockAudit;
  } catch (error) {
    console.log('Error occurred in stockAudit getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockAudit = await transaction.stock_audit.findMany({
      where: {
        is_delete: false,
      },
      include: { project_data: true, site_data: true },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return stockAudit;
  } catch (error) {
    console.log('Error occurred in stockAudit getAll dao', error);
    throw error;
  }
};

const deleteStockAudit = async (stockAuditId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockAudit = await transaction.stock_audit.update({
      where: {
        stock_audit_id: Number(stockAuditId),
      },
      data: {
        is_delete: true,
      },
    });
    return stockAudit;
  } catch (error) {
    console.log('Error occurred in stockAudit deleteStockAudit dao', error);
    throw error;
  }
};

const searchStockAudit = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterStockAudit;
    const stockAudit = await transaction.stock_audit.findMany({
      where: filter,
      include: { project_data: true, site_data: true },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const stockAuditCount = await transaction.stock_audit.count({
      where: filter,
    });
    const stockAuditData = {
      count: stockAuditCount,
      data: stockAudit,
    };
    return stockAuditData;
  } catch (error) {
    console.log('Error occurred in stockAudit dao : searchStockAudit', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteStockAudit,
  searchStockAudit,
};
