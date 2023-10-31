import grnDao from '../dao/grn.dao';
import projectDao from '../dao/project.dao';
import purchaseOrderDao from '../dao/purchaseOrder.dao';
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
      bill_details,
      grn_status,
      created_by,
      grn_details,
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

    if (goods_received_by) {
      const goodsReceivedByExist = await userDao.getById(goods_received_by);
      if (!goodsReceivedByExist) {
        return {
          message: 'goods_received_by does not exist',
          status: false,
          data: null,
        };
      }
    }

    const result = await prisma
      .$transaction(async (prisma) => {
        const grnDetails = await grnDao.add(
          project_id,
          purchase_order_id,
          goods_received_by,
          goods_received_date,
          invoice_id,
          bill_details,
          grn_status,
          created_by,
          grn_details,
          prisma
        );
        return { message: 'success', status: true, data: grnDetails };
      })
      .then((data) => {
        console.log('Successfully GRN Data Returned ');
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

    const filterObj: any = {};

    if (project_id) {
      filterObj.filterGrn = filterObj.filterGrn || {};
      filterObj.filterGrn.AND = filterObj.filterGrn.AND || [];

      filterObj.filterGrn.AND.push({
        project_id: project_id,
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

export { createGrn, getAllGrns, getById, searchGrn };
