import projectDao from '../dao/project.dao';
import siteContractorDao from '../dao/siteContractor.dao';
import stockOutwardDao from '../dao/stockOutward.dao';
import userDao from '../dao/user.dao';
import { stockOutwardBody } from '../interfaces/stockOutward.interface';
import prisma from '../utils/prisma';

/**
 * Method to Create a New StockOutward
 * @param body
 * @returns
 */
const createStockOutward = async (body: stockOutwardBody) => {
  try {
    const {
      project_id,
      site_id,
      site_engineer_id,
      item_count,
      stock_outward_date,
      created_by,
      stock_outward_details,
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

    if (site_id) {
      const siteExist = await siteContractorDao.getBySiteId(site_id);
      if (!siteExist) {
        return {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (site_engineer_id) {
      const siteEngineerExist = await userDao.getById(site_engineer_id);
      if (!siteEngineerExist) {
        return {
          message: 'site_engineer_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const result = await prisma
      .$transaction(async (tx) => {
        const stockOutwardDetails = await stockOutwardDao.add(
          project_id,
          site_id,
          site_engineer_id,
          item_count,
          stock_outward_date,
          created_by,
          stock_outward_details,
          tx
        );
        const result = {
          message: 'success',
          status: true,
          data: stockOutwardDetails,
        };
        return result;
      })
      .then((data) => {
        console.log('Successfully Stock Outward Data Returned ', data);
        return data;
      })
      .catch((error) => {
        console.log('Failure , ROLLBACK was executed ', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in stockOutward service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing StockOutward
 * @param body
 * @returns
 */

const updateStockOutward = async (body: stockOutwardBody) => {
  try {
    const {
      project_id,
      site_id,
      site_engineer_id,
      item_count,
      stock_outward_date,
      stock_outward_details,
      updated_by,
      stock_outward_id,
    } = body;
    let result = null;
    const stockOutwardExist = await stockOutwardDao.getById(stock_outward_id);
    if (!stockOutwardExist) {
      result = {
        message: 'stock_outward_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

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

    if (site_id) {
      const siteExist = await siteContractorDao.getBySiteId(site_id);
      if (!siteExist) {
        return {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (site_engineer_id) {
      const siteEngineerExist = await userDao.getById(site_engineer_id);
      if (!siteEngineerExist) {
        return {
          message: 'site_engineer_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const stockOutwardDetails = await stockOutwardDao.edit(
      project_id,
      site_id,
      site_engineer_id,
      item_count,
      stock_outward_date,
      updated_by,
      stock_outward_details,
      stock_outward_id
    );
    result = { message: 'success', status: true, data: stockOutwardDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in stockOutward service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get StockOutward By StockOutwardId
 * @param stockOutwardId
 * @returns
 */
const getById = async (stockOutwardId: number) => {
  try {
    let result = null;
    const stockOutwardData = await stockOutwardDao.getById(stockOutwardId);
    if (stockOutwardData) {
      result = { message: 'success', status: true, data: stockOutwardData };
      return result;
    } else {
      result = {
        message: 'stock_outward_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById stockOutward service : ', error);
    throw error;
  }
};

/**
 * Method to Get All StockOutwards
 * @returns
 */
const getAllStockOutwards = async () => {
  try {
    const result = await stockOutwardDao.getAll();
    const stockOutwardData = { message: 'success', status: true, data: result };
    return stockOutwardData;
  } catch (error) {
    console.log(
      'Error occurred in getAllStockOutwards stockOutward service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete stockOutward
 * @param stockOutwardId
 */
const deleteStockOutward = async (stockOutwardId: number) => {
  try {
    const stockOutwardExist = await stockOutwardDao.getById(stockOutwardId);

    if (!stockOutwardExist) {
      const result = {
        message: 'stock_outward_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await stockOutwardDao.deleteStockOutward(stockOutwardId);
    if (data) {
      const result = {
        message: 'StockOutward Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this stockOutward',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteStockOutward stockOutward service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search StockOutward - Pagination API
 * @returns
 */
const searchStockOutward = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;
    const site_id = body.site_id;
    const project_id = body.project_id;

    const filterObj: any = {};

    if (status) {
      filterObj.filterStockOutward = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (site_id) {
      filterObj.filterStockOutward = filterObj.filterStockOutward || {};
      filterObj.filterStockOutward.AND = filterObj.filterStockOutward.AND || [];
      filterObj.filterStockOutward.AND.push({
        site_id: site_id,
      });
    }

    if (project_id) {
      filterObj.filterStockOutward = filterObj.filterStockOutward || {};
      filterObj.filterStockOutward.AND = filterObj.filterStockOutward.AND || [];
      filterObj.filterStockOutward.AND.push({
        project_id: project_id,
      });
    }

    if (global_search) {
      filterObj.filterStockOutward = filterObj.filterStockOutward || {};
      filterObj.filterStockOutward.OR = filterObj.filterStockOutward.OR || [];

      filterObj.filterStockOutward.OR.push(
        {
          outward_id: {
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
          site_data: {
            name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          site_engineer_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          site_engineer_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
      filterObj.filterStockOutward.OR.push({
        OR: [
          {
            stock_outward_details: {
              some: {
                item_data: {
                  item_name: {
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

    const result = await stockOutwardDao.searchStockOutward(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempStockOutwardData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempStockOutwardData;
  } catch (error) {
    console.log(
      'Error occurred in searchStockOutward StockOutward service : ',
      error
    );
    throw error;
  }
};

export {
  createStockOutward,
  updateStockOutward,
  getAllStockOutwards,
  getById,
  deleteStockOutward,
  searchStockOutward,
};
