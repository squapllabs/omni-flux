import db from '../utils/db';
import prisma from '../utils/prisma';
import customQueryExecutor from './common/utils.dao';

const add = async (
  vendor_id: number,
  purchase_request_id: number,
  quotation_date: Date,
  quotation_status: string,
  total_quotation_amount: number,
  remarks: string,
  vendor_quotes_documents,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const quotationIdGeneratorQuery = `select concat('VQUO',DATE_PART('year', CURRENT_DATE),'00',nextval('vendor_quotation_sequence')::text) as vendor_quotation_sequence`;

    const quotation_id = await customQueryExecutor.customQueryExecutor(
      quotationIdGeneratorQuery
    );

    const vendorQuotes = await transaction.vendor_quotes.create({
      data: {
        vendor_id,
        purchase_request_id,
        quotation_date,
        quotation_status,
        total_quotation_amount,
        remarks,
        quotation_id: quotation_id[0].vendor_quotation_sequence,
        vendor_quotes_documents,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return vendorQuotes;
  } catch (error) {
    console.log('Error occurred in vendorQuotesDao add', error);
    throw error;
  }
};

const edit = async (
  vendor_quotes_id: number,
  vendor_id: number,
  purchase_request_id: number,
  quotation_date: Date,
  quotation_status: string,
  total_quotation_amount: number,
  remarks: string,
  updated_by: number,
  vendor_quotes_documents,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotes = await transaction.vendor_quotes.update({
      where: {
        vendor_quotes_id: Number(vendor_quotes_id),
      },
      data: {
        vendor_id,
        purchase_request_id,
        quotation_date,
        quotation_status,
        total_quotation_amount,
        remarks,
        updated_by,
        vendor_quotes_documents,
        updated_date: currentDate,
      },
    });
    return vendorQuotes;
  } catch (error) {
    console.log('Error occurred in vendorQuotesDao edit', error);
    throw error;
  }
};

const getById = async (vendorQuotesId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotes = await transaction.vendor_quotes.findFirst({
      where: {
        vendor_quotes_id: Number(vendorQuotesId),
        is_delete: false,
      },
      include: {
        vendor_data: true,
        purchase_request_data: true,
      },
    });
    return vendorQuotes;
  } catch (error) {
    console.log('Error occurred in vendorQuotes getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotes = await transaction.vendor_quotes.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return vendorQuotes;
  } catch (error) {
    console.log('Error occurred in vendorQuotes getAll dao', error);
    throw error;
  }
};

const deleteVendorQuotes = async (
  vendorQuotesId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotes = await transaction.vendor_quotes.update({
      where: {
        vendor_quotes_id: Number(vendorQuotesId),
      },
      data: {
        is_delete: true,
      },
    });
    return vendorQuotes;
  } catch (error) {
    console.log('Error occurred in vendorQuotes deletevendorQuotes dao', error);
    throw error;
  }
};

const searchVendorQuotes = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  global_search,
  status,
  purchase_request_id
) => {
  try {
    const is_delete = status === 'AC' ? false : true;
    const query = `select
                    vq.*,
                    v.vendor_name
                from
                    vendor_quotes vq
                left join vendor v on
                    v.vendor_id = vq.vendor_id
                where
                    (v.vendor_name ilike '%${global_search}%'
                        or vq.quotation_status ilike '%${global_search}%'
                        or vq.remarks ilike '%${global_search}%'
                        or vq.quotation_id ilike '%${global_search}%' )
                    and (vq.is_delete = ${is_delete} and vq.purchase_request_id =${purchase_request_id})
                order by
                    vq.${orderByColumn} ${orderByDirection}
                limit ${limit} offset ${offset}`;

    const countQuery = `select
                    count(vq.*)
                from
                    vendor_quotes vq
                left join vendor v on
                    v.vendor_id = vq.vendor_id
                where
                    (v.vendor_name ilike '%${global_search}%'
                        or vq.quotation_status ilike '%${global_search}%'
                        or vq.remarks ilike '%${global_search}%'
                        or vq.quotation_id ilike '%${global_search}%' )
                    and (vq.is_delete = ${is_delete} and vq.purchase_request_id =${purchase_request_id})`;

    const result = await customQueryExecutor.customQueryExecutor(query);
    const count = await customQueryExecutor.customQueryExecutor(countQuery);

    const vendorQuotesData = {
      count: Number(count[0].count),
      data: result,
    };
    return vendorQuotesData;
  } catch (error) {
    console.log(
      'Error occurred in vendorQuotes dao : searchvendorQuotes',
      error
    );
    throw error;
  }
};

const updateStatusAndDocument = async (
  vendor_quotes_id: number,
  quotation_status: string,
  updated_by: number,
  vendor_quotes_documents,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotes = await transaction.vendor_quotes.update({
      where: {
        vendor_quotes_id: Number(vendor_quotes_id),
      },
      data: {
        quotation_status,
        updated_by,
        vendor_quotes_documents,
        updated_date: currentDate,
      },
    });
    return vendorQuotes;
  } catch (error) {
    console.log(
      'Error occurred in vendorQuotesDao updateStatusAndDocument',
      error
    );
    throw error;
  }
};

const getByPurchaseRequestIdAndVendorId = async (
  purchase_request_id: number,
  vendor_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotes = await transaction.vendor_quotes.findFirst({
      where: {
        purchase_request_id: Number(purchase_request_id),
        vendor_id: Number(vendor_id),
        is_delete: false,
      },
    });
    return vendorQuotes;
  } catch (error) {
    console.log(
      'Error occurred in vendorQuotes getByPurchaseRequestIdAndVendorId dao',
      error
    );
    throw error;
  }
};

const getByPurchaseRequestId = async (
  purchase_request_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const vendorQuotes = await transaction.vendor_quotes.findMany({
      where: {
        purchase_request_id: Number(purchase_request_id),
        is_delete: false,
      },
      include: {
        vendor_data: true,
        purchase_request_data: true,
      },
    });
    return vendorQuotes;
  } catch (error) {
    console.log(
      'Error occurred in vendorQuotes getByPurchaseRequestId dao',
      error
    );
    throw error;
  }
};

const getVendorDetailsByPurchaseRequestId = async (
  purchase_request_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `select
      v.*,
      vq.*
    from
      vendor_quotes vq
    left join vendor v on
      vq.vendor_id = v.vendor_id
    where
      purchase_request_id = $1`;
    const vendorQuotes = await transaction.manyOrNone(query, [
      purchase_request_id,
    ]);
    return vendorQuotes;
  } catch (error) {
    console.log(
      'Error occurred in vendorQuotes getVendorDetailsByPurchaseRequestId dao',
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
  deleteVendorQuotes,
  searchVendorQuotes,
  updateStatusAndDocument,
  getByPurchaseRequestIdAndVendorId,
  getByPurchaseRequestId,
  getVendorDetailsByPurchaseRequestId,
};
