import prisma from '../utils/prisma';

const add = async (
  purchase_order_id: number,
  grn_id: number,
  invoice_number: string,
  invoice_document: JSON,
  requested_by: number,
  invoice_date: Date,
  due_date: Date,
  status: string,
  additional_info: JSON,
  total_amount: number,
  paid_by: number,
  paid_date: Date,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_invoice_date = invoice_date ? new Date(invoice_date) : null;
    const formatted_due_date = due_date ? new Date(due_date) : null;
    const formatted_paid_date = paid_date ? new Date(paid_date) : null;
    const purchaseOrderInvoice =
      await transaction.purchase_order_invoice.create({
        data: {
          purchase_order_id,
          grn_id,
          invoice_number,
          invoice_document,
          requested_by,
          invoice_date: formatted_invoice_date,
          due_date: formatted_due_date,
          status,
          additional_info,
          total_amount,
          paid_by,
          paid_date: formatted_paid_date,
          created_by,
          created_date: currentDate,
          updated_date: currentDate,
        },
      });
    return purchaseOrderInvoice;
  } catch (error) {
    console.log('Error occurred in purchaseOrderInvoiceDao add', error);
    throw error;
  }
};

const edit = async (
  purchase_order_id: number,
  grn_id: number,
  invoice_number: string,
  invoice_document: JSON,
  requested_by: number,
  invoice_date: Date,
  due_date: Date,
  status: string,
  additional_info: JSON,
  total_amount: number,
  paid_by: number,
  paid_date: Date,
  updated_by: number,
  purchase_order_invoice_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_invoice_date = invoice_date ? new Date(invoice_date) : null;
    const formatted_due_date = due_date ? new Date(due_date) : null;
    const formatted_paid_date = paid_date ? new Date(paid_date) : null;
    const purchaseOrderInvoice =
      await transaction.purchase_order_invoice.update({
        where: {
          purchase_order_invoice_id: purchase_order_invoice_id,
        },
        data: {
          purchase_order_id,
          grn_id,
          invoice_number,
          invoice_document,
          requested_by,
          invoice_date: formatted_invoice_date,
          due_date: formatted_due_date,
          status,
          additional_info,
          total_amount,
          paid_by,
          paid_date: formatted_paid_date,
          updated_by,
          updated_date: currentDate,
        },
      });
    return purchaseOrderInvoice;
  } catch (error) {
    console.log('Error occurred in purchaseOrderInvoiceDao edit', error);
    throw error;
  }
};

const getById = async (
  purchaseOrderInvoiceId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderInvoice =
      await transaction.purchase_order_invoice.findFirst({
        where: {
          purchase_order_invoice_id: Number(purchaseOrderInvoiceId),
        },
        include: {
          purchase_order_data: true,
          grn_data: true,
          requested_by_data: {
            select: {
              first_name: true,
              last_name: true,
              contact_no: true,
              email_id: true,
            },
          },
          paid_by_data: {
            select: {
              first_name: true,
              last_name: true,
              contact_no: true,
              email_id: true,
            },
          },
        },
      });
    return purchaseOrderInvoice;
  } catch (error) {
    console.log('Error occurred in purchaseOrderInvoice getById dao', error);
    throw error;
  }
};

const getByPOId = async (purchaseOrderId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderInvoice =
      await transaction.purchase_order_invoice.findMany({
        where: {
          purchase_order_id: Number(purchaseOrderId),
        },
        orderBy: [{ created_date: 'asc' }],
        include: {
          purchase_order_data: true,
          grn_data: true,
          requested_by_data: {
            select: {
              first_name: true,
              last_name: true,
              contact_no: true,
              email_id: true,
            },
          },
          paid_by_data: {
            select: {
              first_name: true,
              last_name: true,
              contact_no: true,
              email_id: true,
            },
          },
        },
      });
    return purchaseOrderInvoice;
  } catch (error) {
    console.log('Error occurred in purchaseOrderInvoice getByPOId dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderInvoice =
      await transaction.purchase_order_invoice.findMany({
        include: {
          purchase_order_data: true,
          grn_data: true,
          requested_by_data: {
            select: {
              first_name: true,
              last_name: true,
              contact_no: true,
              email_id: true,
            },
          },
          paid_by_data: {
            select: {
              first_name: true,
              last_name: true,
              contact_no: true,
              email_id: true,
            },
          },
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
    return purchaseOrderInvoice;
  } catch (error) {
    console.log('Error occurred in purchaseOrderInvoice getAll dao', error);
    throw error;
  }
};

const deletePurchaseOrderInvoice = async (
  purchaseOrderInvoiceId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrderInvoice =
      await transaction.purchase_order_invoice.delete({
        where: {
          purchase_order_invoice_id: Number(purchaseOrderInvoiceId),
        },
      });
    return purchaseOrderInvoice;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderInvoice deletePurchaseOrderInvoice dao',
      error
    );
    throw error;
  }
};

const searchPurchaseOrderInvoice = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterPurchaseOrderInvoice;
    const checkDataAvailability =
      await transaction.purchase_order_invoice.findMany({});
    if (checkDataAvailability.length > 0) {
      const purchaseOrderInvoice =
        await transaction.purchase_order_invoice.findMany({
          where: filter,
          include: {
            purchase_order_data: true,
            grn_data: true,
            requested_by_data: {
              select: {
                first_name: true,
                last_name: true,
                contact_no: true,
                email_id: true,
              },
            },
            paid_by_data: {
              select: {
                first_name: true,
                last_name: true,
                contact_no: true,
                email_id: true,
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
      const purchaseOrderInvoiceCount =
        await transaction.purchase_order_invoice.count({
          where: filter,
        });
      const purchaseOrderInvoiceData = {
        count: purchaseOrderInvoiceCount,
        data: purchaseOrderInvoice,
      };
      return purchaseOrderInvoiceData;
    } else {
      return checkDataAvailability;
    }
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderInvoice dao : searchPurchaseOrderInvoice',
      error
    );
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getByPOId,
  getAll,
  deletePurchaseOrderInvoice,
  searchPurchaseOrderInvoice,
};
