import prisma from '../utils/prisma';

const add = async (
  purchase_order_id: number,
  item_id: number,
  order_quantity: number,
  inward_quantity: number,
  unit_price: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const inward_remaining_quantity = order_quantity - inward_quantity;
    const purchaseOrderItem = await transaction.purchase_order_item.create({
      data: {
        purchase_order_id,
        item_id,
        order_quantity,
        inward_quantity,
        inward_remaining_quantity: inward_remaining_quantity,
        unit_price,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return purchaseOrderItem;
  } catch (error) {
    console.log('Error occurred in purchaseOrderItemDao add', error);
    throw error;
  }
};

const edit = async (
  purchase_order_id: number,
  item_id: number,
  order_quantity: number,
  inward_quantity: number,
  unit_price: number,
  updated_by: number,
  purchase_order_item_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const inward_remaining_quantity = order_quantity - inward_quantity;
    const purchaseOrderItem = await transaction.purchase_order_item.update({
      where: {
        purchase_order_item_id: purchase_order_item_id,
      },
      data: {
        purchase_order_id,
        item_id,
        order_quantity,
        inward_quantity,
        inward_remaining_quantity: inward_remaining_quantity,
        unit_price,
        updated_by,
        updated_date: currentDate,
      },
    });
    return purchaseOrderItem;
  } catch (error) {
    console.log('Error occurred in purchaseOrderItemDao edit', error);
    throw error;
  }
};

const getById = async (purchaseOrderItemId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderItem = await transaction.purchase_order_item.findFirst({
      where: {
        purchase_order_item_id: Number(purchaseOrderItemId),
        is_delete: false,
      },
      include: {
        purchase_order_data: true,
        item_data: true,
      },
    });
    return purchaseOrderItem;
  } catch (error) {
    console.log('Error occurred in purchaseOrderItem getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderItem = await transaction.purchase_order_item.findMany({
      where: {
        is_delete: false,
      },
      include: {
        purchase_order_data: true,
        item_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return purchaseOrderItem;
  } catch (error) {
    console.log('Error occurred in purchaseOrderItem getAll dao', error);
    throw error;
  }
};

const deletePurchaseOrderItem = async (
  purchaseOrderItemId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderItem = await transaction.purchase_order_item.update({
      where: {
        purchase_order_item_id: Number(purchaseOrderItemId),
      },
      data: {
        is_delete: true,
      },
    });
    return purchaseOrderItem;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderItem deletePurchaseOrderItem dao',
      error
    );
    throw error;
  }
};

const searchPurchaseOrderItem = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterPurchaseOrderItem;
    const purchaseOrderItem = await transaction.purchase_order_item.findMany({
      where: filter,
      include: {
        purchase_order_data: true,
        item_data: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const purchaseOrderItemCount = await transaction.purchase_order_item.count({
      where: filter,
    });
    const purchaseOrderItemData = {
      count: purchaseOrderItemCount,
      data: purchaseOrderItem,
    };
    return purchaseOrderItemData;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderItem dao : searchPurchaseOrderItem',
      error
    );
    throw error;
  }
};

const getByPurchaseOrderId = async (
  purchaseOrderId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderItem = await transaction.purchase_order_item.findMany({
      where: {
        purchase_order_id: Number(purchaseOrderId),
        is_delete: false,
      },
      include: {
        purchase_order_data: true,
        item_data: true,
      },
    });
    return purchaseOrderItem;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderItem getByPurchaseOrderId dao',
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
  deletePurchaseOrderItem,
  searchPurchaseOrderItem,
  getByPurchaseOrderId,
};
