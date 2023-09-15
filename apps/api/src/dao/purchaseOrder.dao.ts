import prisma from '../utils/prisma';

const add = async (
  purchase_request_id: number,
  vendor_id: number,
  order_date: Date,
  status: string,
  total_cost: number,
  order_remark: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_order_date = order_date ? new Date(order_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.create({
      data: {
        purchase_request_id,
        vendor_id,
        order_date: formatted_order_date,
        status,
        total_cost,
        order_remark,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrderDao add', error);
    throw error;
  }
};

const edit = async (
  purchase_request_id: number,
  vendor_id: number,
  order_date: Date,
  status: string,
  total_cost: number,
  order_remark: string,
  updated_by: number,
  purchase_order_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_order_date = order_date ? new Date(order_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.update({
      where: {
        purchase_order_id: purchase_order_id,
      },
      data: {
        purchase_request_id,
        vendor_id,
        order_date: formatted_order_date,
        status,
        total_cost,
        order_remark,
        updated_by,
        updated_date: currentDate,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrderDao edit', error);
    throw error;
  }
};

const getById = async (purchaseOrderId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.findFirst({
      where: {
        purchase_order_id: Number(purchaseOrderId),
        is_delete: false,
      },
      include: {
        purchase_request_data: { include: { indent_request_data: true } },
        vendor_data: true,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrder getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.findMany({
      where: {
        is_delete: false,
      },
      include: {
        purchase_request_data: { include: { indent_request_data: true } },
        vendor_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrder getAll dao', error);
    throw error;
  }
};

const deletePurchaseOrder = async (
  purchaseOrderId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.update({
      where: {
        purchase_order_id: Number(purchaseOrderId),
      },
      data: {
        is_delete: true,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrder deletePurchaseOrder dao',
      error
    );
    throw error;
  }
};

const searchPurchaseOrder = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterPurchaseOrder;
    const purchaseOrder = await transaction.purchase_order.findMany({
      where: filter,
      include: {
        purchase_request_data: { include: { indent_request_data: true } },
        vendor_data: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const purchaseOrderCount = await transaction.purchase_order.count({
      where: filter,
    });
    const purchaseOrderData = {
      count: purchaseOrderCount,
      data: purchaseOrder,
    };
    return purchaseOrderData;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrder dao : searchPurchaseOrder',
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
  deletePurchaseOrder,
  searchPurchaseOrder,
};
