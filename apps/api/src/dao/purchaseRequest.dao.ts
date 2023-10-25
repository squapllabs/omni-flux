import prisma from '../utils/prisma';
import customQueryExecutor from './common/utils.dao';

const add = async (
  indent_request_id: number,
  requester_user_id: number,
  request_date: Date,
  status: string,
  vendor_selection_method: string,
  project_id: number,
  site_id: number,
  selected_vendor_id: number,
  total_cost: number,
  created_by: number,
  vendor_ids: Array<number>,
  purchase_request_details,
  purchase_request_documents,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_request_date = request_date ? new Date(request_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const purchaseRequestCodeGeneratorQuery = `select concat('PUR',DATE_PART('year', CURRENT_DATE),'00',nextval('purchase_request_code_sequence')::text) as purchase_request_code_sequence`;

    const purchaseRequestCode = await customQueryExecutor.customQueryExecutor(
      purchaseRequestCodeGeneratorQuery
    );

    const purchaseRequest = await transaction.purchase_request.create({
      data: {
        indent_request_id,
        requester_user_id,
        request_date: formatted_request_date,
        status,
        vendor_selection_method,
        project_id,
        site_id,
        selected_vendor_id,
        total_cost,
        created_by,
        purchase_request_details,
        purchase_request_documents,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
        purchase_request_code:
          purchaseRequestCode[0].purchase_request_code_sequence,
      },
    });

    const new_purchase_request_id = purchaseRequest?.purchase_request_id;

    const vendorQuotesDetails = [];
    const quotationIdGeneratorQuery = `select concat('VQUO',DATE_PART('year', CURRENT_DATE),'00',nextval('vendor_quotation_sequence')::text) as vendor_quotation_sequence`;

    for (const vendor of vendor_ids) {
      const vendor_id = vendor;
      const quotation_id = await customQueryExecutor.customQueryExecutor(
        quotationIdGeneratorQuery
      );

      const vendorExistForThisPurchaseRequest =
        await transaction.vendor_quotes.findFirst({
          where: {
            purchase_request_id: new_purchase_request_id,
            vendor_id: vendor_id,
            is_delete: false,
          },
        });
      if (!vendorExistForThisPurchaseRequest) {
        const vendorQuotes = await transaction.vendor_quotes.create({
          data: {
            vendor_id: vendor_id,
            purchase_request_id: new_purchase_request_id,
            quotation_date: formatted_request_date,
            quotation_status: 'Pending',
            total_quotation_amount: 0,
            remarks: null,
            quotation_details: purchase_request_details,
            quotation_id: quotation_id[0].vendor_quotation_sequence,
            created_by,
            created_date: currentDate,
            updated_date: currentDate,
            is_delete: is_delete,
          },
        });
        vendorQuotesDetails.push(vendorQuotes);
      } else {
        vendorQuotesDetails.push({
          message: `This vendor_id - ${vendor_id} is already exists for this Purchase Request`,
          status: false,
          existing_data: vendorExistForThisPurchaseRequest,
        });
      }
    }

    const purchaseRequestData = {
      purchase_request: purchaseRequest,
      vendor_quotes: vendorQuotesDetails,
    };

    return purchaseRequestData;
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
  site_id: number,
  selected_vendor_id: number,
  total_cost: number,
  updated_by: number,
  purchase_request_details: JSON,
  purchase_request_documents,
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
        site_id,
        selected_vendor_id,
        total_cost,
        updated_by,
        purchase_request_details,
        purchase_request_documents,
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
        vendor_quotes: {
          include: {
            vendor_data: true,
          },
        },
        indent_request_data: true,
        requester_user_data: {
          select: {
            first_name: true,
            last_name: true,
            contact_no: true,
            email_id: true,
          },
        },
        project_data: true,
        selected_vendor_data: true,
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

const updateVendor = async (
  status: string,
  selected_vendor_id: number,
  updated_by: number,
  total_cost: number,
  purchase_request_documents,
  purchase_request_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequest = await transaction.purchase_request.update({
      where: {
        purchase_request_id: purchase_request_id,
      },
      data: {
        status,
        selected_vendor_id,
        total_cost,
        purchase_request_documents,
        updated_by,
        updated_date: currentDate,
      },
    });
    return purchaseRequest;
  } catch (error) {
    console.log('Error occurred in purchaseRequestDao updateVendor', error);
    throw error;
  }
};

const getAllPurchaseRequestProjectsByStatus = async (
  status: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseRequest = await transaction.purchase_request.findMany({
      where: {
        status: status,
        is_delete: false,
      },
      distinct: ['project_id'],
      select: {
        project_data: true,
      },
    });
    return purchaseRequest;
  } catch (error) {
    console.log(
      'Error occurred in purchaseRequest getAllPurchaseRequestProjectsByStatus dao',
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
  updateVendor,
  getAllPurchaseRequestProjectsByStatus,
};
