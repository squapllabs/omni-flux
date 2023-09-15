import prisma from '../utils/prisma';

const add = async (
  indent_request_id: number,
  requester_user_id: number,
  request_date: Date,
  status: string,
  vendor_selection_method: string,
  project_id: number,
  selected_vendor_id: number,
  total_cost: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_request_date = request_date ? new Date(request_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequest = await transaction.purchase_request.create({
      data: {
        indent_request_id,
        requester_user_id,
        request_date: formatted_request_date,
        status,
        vendor_selection_method,
        project_id,
        selected_vendor_id,
        total_cost,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return purchaseRequest;
  } catch (error) {
    console.log('Error occurred in purchaseRequestDao add', error);
    throw error;
  }
};

const edit = async (
  indent_request_id: number,
  requester_user_id: number,
  request_date: Date,
  status: string,
  vendor_selection_method: string,
  project_id: number,
  selected_vendor_id: number,
  total_cost: number,
  updated_by: number,
  purchase_request_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_request_date = request_date ? new Date(request_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequest = await transaction.purchase_request.update({
      where: {
        purchase_request_id: purchase_request_id,
      },
      data: {
        indent_request_id,
        requester_user_id,
        request_date: formatted_request_date,
        status,
        vendor_selection_method,
        project_id,
        selected_vendor_id,
        total_cost,
        updated_by,
        updated_date: currentDate,
      },
    });
    return purchaseRequest;
  } catch (error) {
    console.log('Error occurred in purchaseRequestDao edit', error);
    throw error;
  }
};

const getById = async (purchaseRequestId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequest = await transaction.purchase_request.findFirst({
      where: {
        purchase_request_id: Number(purchaseRequestId),
        is_delete: false,
      },
      include: {
        indent_request_data: true,
        requester_user_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        selected_vendor_data: { select: { vendor_name: true } },
      },
    });
    return purchaseRequest;
  } catch (error) {
    console.log('Error occurred in purchaseRequest getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequest = await transaction.purchase_request.findMany({
      where: {
        is_delete: false,
      },
      include: {
        indent_request_data: true,
        requester_user_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        selected_vendor_data: { select: { vendor_name: true } },
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return purchaseRequest;
  } catch (error) {
    console.log('Error occurred in purchaseRequest getAll dao', error);
    throw error;
  }
};

const deletePurchaseRequest = async (
  purchaseRequestId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequest = await transaction.purchase_request.update({
      where: {
        purchase_request_id: Number(purchaseRequestId),
      },
      data: {
        is_delete: true,
      },
    });
    return purchaseRequest;
  } catch (error) {
    console.log(
      'Error occurred in purchaseRequest deletePurchaseRequest dao',
      error
    );
    throw error;
  }
};

const searchPurchaseRequest = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterPurchaseRequest;
    const purchaseRequest = await transaction.purchase_request.findMany({
      where: filter,
      include: {
        indent_request_data: true,
        requester_user_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        selected_vendor_data: { select: { vendor_name: true } },
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const purchaseRequestCount = await transaction.purchase_request.count({
      where: filter,
    });
    const purchaseRequestData = {
      count: purchaseRequestCount,
      data: purchaseRequest,
    };
    return purchaseRequestData;
  } catch (error) {
    console.log(
      'Error occurred in purchaseRequest dao : searchPurchaseRequest',
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
  deletePurchaseRequest,
  searchPurchaseRequest,
};