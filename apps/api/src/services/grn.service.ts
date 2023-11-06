import expenseDao from '../dao/expense.dao';
import grnDao from '../dao/grn.dao';
import masterDataDao from '../dao/masterData.dao';
import projectDao from '../dao/project.dao';
import projectInventoryDao from '../dao/projectInventory.dao';
import purchaseOrderDao from '../dao/purchaseOrder.dao';
import purchaseOrderInvoiceDao from '../dao/purchaseOrderInvoice.dao';
import purchaseOrderItemDao from '../dao/purchaseOrderItem.dao';
import userDao from '../dao/user.dao';
import { grnBody } from '../interfaces/grn.interface';
import prisma from '../utils/prisma';

/**
 * Method to Create a New Grn
 * @param body
 * @returns
 */
const createGrn = async (body: grnBody) => {
  try {
    const {
      project_id,
      purchase_order_id,
      goods_received_by,
      goods_received_date,
      invoice_id,
      invoice_amount,
      notes,
      bill_details,
      grn_status,
      created_by,
      grn_details,
      site_id,
      purchase_order_type,
      is_product_received,
    } = body;

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (purchase_order_id) {
      const purchaseOrderExist = await purchaseOrderDao.getById(
        purchase_order_id
      );
      if (!purchaseOrderExist) {
        return {
          message: 'purchase_order_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    let goodsReceivedByExist = null;
    if (goods_received_by) {
      goodsReceivedByExist = await userDao.getById(goods_received_by);
      if (!goodsReceivedByExist) {
        return {
          message: 'goods_received_by does not exist',
          status: false,
          data: null,
        };
      }
    }

    const result = await prisma
      .$transaction(
        async (prisma) => {
          /* GRN */
          const grnDetails = await grnDao.add(
            project_id,
            purchase_order_id,
            goods_received_by,
            goods_received_date,
            invoice_id,
            invoice_amount,
            notes,
            bill_details,
            grn_status,
            created_by,
            grn_details,
            prisma
          );

          /*Purchase Order - Status Updation */
          await purchaseOrderDao.updateStatusByPOId(
            is_product_received ? 'Product Received' : 'Partially Received',
            created_by,
            purchase_order_id,
            prisma
          );

          /* Purchase Order Item */
          let purchaseOrderItem = null;
          let projectInventoryData = null;

          if (grn_details.length > 0) {
            for await (const grnDetail of grn_details) {
              const {
                item_id,
                previously_received_quantity,
                currently_received_quantity,
                purchase_order_item_id,
                order_quantity,
                unit_price,
              } = grnDetail;

              const received_quantity =
                previously_received_quantity + currently_received_quantity;

              purchaseOrderItem = await purchaseOrderItemDao.edit(
                purchase_order_id,
                item_id,
                order_quantity,
                received_quantity,
                unit_price,
                created_by,
                purchase_order_item_id,
                prisma
              );

              /* Project Inventory */
              const projectInventoryDetails =
                await projectInventoryDao.getByProjectSiteItem(
                  project_id,
                  site_id,
                  item_id,
                  prisma
                );
              if (projectInventoryDetails) {
                const latest_quantity =
                  received_quantity +
                  projectInventoryDetails?.available_quantity;
                const total_cost = unit_price * latest_quantity;
                projectInventoryData =
                  await projectInventoryDao.updateQuantityByProjectInventoryId(
                    latest_quantity,
                    created_by,
                    total_cost,
                    projectInventoryDetails?.project_inventory_id,
                    prisma
                  );
              } else {
                const total_cost = unit_price * received_quantity;
                projectInventoryData = await projectInventoryDao.add(
                  project_id,
                  item_id,
                  unit_price,
                  received_quantity,
                  total_cost,
                  created_by,
                  site_id,
                  prisma
                );
              }
            }
          }

          /* Expense and Expense Details */

          let expenseAndExpenseDetailsData = null;

          if (purchase_order_type === 'Local Purchase') {
            const expenseData = await masterDataDao.getByMasterDataName(
              'Local Purchase'
            );

            grn_details.forEach((value) => {
              value.expense_data_id = expenseData?.master_data_id;
              value.bill_number = invoice_id;
              value.bill_details = bill_details;
              value.total =
                value.currently_received_quantity * value.unit_price;
              value.description = 'Expense for Local Purchase';
              value.quantity = value.currently_received_quantity;
              value.unit_value = value.unit_price;
              value.bill_type = 'Local Purchase';
              value.is_delete = false;
              value.status = 'Pending';
            });

            let expense_total_amount = 0;

            for (const expense of grn_details) {
              expense_total_amount += expense.total;
            }

            expenseAndExpenseDetailsData = await expenseDao.add(
              site_id,
              project_id,
              goodsReceivedByExist?.first_name +
                ' ' +
                goodsReceivedByExist?.last_name,
              null,
              goodsReceivedByExist?.contact_no,
              'Local Purchase Order Regarding',
              'Purchase',
              'Site Engineer',
              null,
              null,
              new Date(),
              bill_details,
              created_by,
              'Pending',
              expense_total_amount,
              grn_details,
              goods_received_by,
              'Local Purchase',
              prisma
            );

            /* Purchase Order Invoice */
            await purchaseOrderInvoiceDao.add(
              purchase_order_id,
              grnDetails?.grn?.grn_id,
              invoice_id,
              bill_details,
              goods_received_by,
              new Date(),
              null,
              'To Be Paid',
              undefined,
              invoice_amount,
              null,
              null,
              created_by,
              prisma
            );
          }

          const result = {
            grn_with_grn_details: grnDetails,
            purchase_order_item: purchaseOrderItem,
            project_inventory: projectInventoryData,
            expense_with_expense_details: expenseAndExpenseDetailsData,
          };

          return { message: 'success', status: true, data: result };
        },
        {
          timeout: Number(process.env.TRANSACTION_TIMEOUT),
        }
      )
      .then((data) => {
        console.log('Successfully GRN Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in grn service Add: ', error);
    throw error;
  }
};

/**
 * Method to get Grn By GrnId
 * @param grnId
 * @returns
 */
const getById = async (grnId: number) => {
  try {
    let result = null;
    const grnData = await grnDao.getById(grnId);
    if (grnData) {
      result = { message: 'success', status: true, data: grnData };
      return result;
    } else {
      result = {
        message: 'grn_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById grn service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Grns
 * @returns
 */
const getAllGrns = async () => {
  try {
    const result = await grnDao.getAll();
    const grnData = { message: 'success', status: true, data: result };
    return grnData;
  } catch (error) {
    console.log('Error occurred in getAllGrns grn service : ', error);
    throw error;
  }
};

/**
 * Method to search Grn - Pagination API
 * @returns
 */
const searchGrn = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const project_id = body.project_id;
    const purchase_order_id = body.purchase_order_id;

    const filterObj: any = {};

    if (project_id) {
      filterObj.filterGrn = filterObj.filterGrn || {};
      filterObj.filterGrn.AND = filterObj.filterGrn.AND || [];

      filterObj.filterGrn.AND.push({
        project_id: project_id,
      });
    }

    if (purchase_order_id) {
      filterObj.filterGrn = filterObj.filterGrn || {};
      filterObj.filterGrn.AND = filterObj.filterGrn.AND || [];

      filterObj.filterGrn.AND.push({
        purchase_order_id: purchase_order_id,
      });
    }

    if (global_search) {
      filterObj.filterGrn = filterObj.filterGrn || {};
      filterObj.filterGrn.OR = filterObj.filterGrn.OR || [];

      filterObj.filterGrn.OR.push(
        {
          invoice_id: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          grn_status: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          project_data: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          goods_received_by_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );

      filterObj.filterGrn.OR.push({
        OR: [
          {
            expense_details: {
              some: {
                progressed_by_data: {
                  first_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                  last_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
          {
            expense_details: {
              some: {
                expense_master_data: {
                  master_data_name: {
                    contains: global_search,
                    mode: 'insensitive',
                  },
                },
              },
            },
          },
        ],
      });
    }

    const result = await grnDao.searchGrn(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );
    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    if (result.count >= 0) {
      const tempGrnData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempGrnData;
    } else {
      const tempGrnData = {
        message: 'No data found',
        status: false,
        is_available: false,
      };
      return tempGrnData;
    }
  } catch (error) {
    console.log('Error occurred in searchGrn Grn service : ', error);
    throw error;
  }
};

/**
 * Method to get Grn By purchase_order_id
 * @param purchase_order_id
 * @returns
 */
const getByPurchaseOrderId = async (purchase_order_id: number) => {
  try {
    const purchaseOrderExist = await purchaseOrderDao.getById(
      purchase_order_id
    );
    if (!purchaseOrderExist) {
      return {
        message: 'purchase_order_id does not exist',
        status: false,
        data: null,
      };
    }
    const grnData = await grnDao.getByPurchaseOrderId(purchase_order_id);
    if (grnData.length > 0) {
      return { message: 'success', status: true, data: grnData };
    } else {
      return {
        message: 'No data found for this purchase_order_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log('Error occurred in getByPurchaseOrderId grn service : ', error);
    throw error;
  }
};

export { createGrn, getAllGrns, getById, searchGrn, getByPurchaseOrderId };
