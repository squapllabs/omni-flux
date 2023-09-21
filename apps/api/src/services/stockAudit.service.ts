import stockAuditDao from '../dao/stockAudit.dao';
import { stockAuditBody } from '../interfaces/stockAudit.interface';
import projectDao from '../dao/project.dao';
import siteContractorDao from '../dao/siteContractor.dao';

/**
 * Method to Create a New StockAudit
 * @param body
 * @returns
 */
const createStockAudit = async (body: stockAuditBody) => {
  try {
    const { project_id, site_id, stock_audit_date, item_details, created_by } =
      body;

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

    const stockAuditDetails = await stockAuditDao.add(
      project_id,
      site_id,
      stock_audit_date,
      item_details,
      created_by
    );
    const result = {
      message: 'success',
      status: true,
      data: stockAuditDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in stockAudit service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing StockAudit
 * @param body
 * @returns
 */

const updateStockAudit = async (body: stockAuditBody) => {
  try {
    const {
      project_id,
      site_id,
      stock_audit_date,
      item_details,
      updated_by,
      stock_audit_id,
    } = body;
    let result = null;
    const stockAuditExist = await stockAuditDao.getById(stock_audit_id);
    if (!stockAuditExist) {
      result = {
        message: 'stock_audit_id does not exist',
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

    const stockAuditDetails = await stockAuditDao.edit(
      project_id,
      site_id,
      stock_audit_date,
      item_details,
      updated_by,
      stock_audit_id
    );
    result = { message: 'success', status: true, data: stockAuditDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in stockAudit service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get StockAudit By StockAuditId
 * @param stockAuditId
 * @returns
 */
const getById = async (stockAuditId: number) => {
  try {
    let result = null;
    const stockAuditData = await stockAuditDao.getById(stockAuditId);
    if (stockAuditData) {
      result = { message: 'success', status: true, data: stockAuditData };
      return result;
    } else {
      result = {
        message: 'stock_audit_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById stockAudit service : ', error);
    throw error;
  }
};

/**
 * Method to Get All StockAudits
 * @returns
 */
const getAllStockAudits = async () => {
  try {
    const result = await stockAuditDao.getAll();
    const stockAuditData = { message: 'success', status: true, data: result };
    return stockAuditData;
  } catch (error) {
    console.log(
      'Error occurred in getAllStockAudits stockAudit service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete stockAudit
 * @param stockAuditId
 */
const deleteStockAudit = async (stockAuditId: number) => {
  try {
    const stockAuditExist = await stockAuditDao.getById(stockAuditId);

    if (!stockAuditExist) {
      const result = {
        message: 'stock_audit_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await stockAuditDao.deleteStockAudit(stockAuditId);
    if (data) {
      const result = {
        message: 'StockAudit Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this stockAudit',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteStockAudit stockAudit service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search StockAudit - Pagination API
 * @returns
 */
const searchStockAudit = async (body) => {
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
    const project_id = body.project_id;
    const site_id = body.site_id;

    const filterObj: any = {};

    if (status) {
      filterObj.filterStockAudit = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (project_id) {
      filterObj.filterStockAudit = filterObj.filterStockAudit || {};
      filterObj.filterStockAudit.AND = filterObj.filterStockAudit.AND || [];

      filterObj.filterStockAudit.AND.push({
        project_id: project_id,
      });
    }

    if (site_id) {
      filterObj.filterStockAudit = filterObj.filterStockAudit || {};
      filterObj.filterStockAudit.AND = filterObj.filterStockAudit.AND || [];

      filterObj.filterStockAudit.AND.push({
        site_id: site_id,
      });
    }

    if (global_search) {
      filterObj.filterStockAudit = filterObj.filterStockAudit || {};
      filterObj.filterStockAudit.OR = filterObj.filterStockAudit.OR || [];

      filterObj.filterStockAudit.OR.push(
        {
          project_data: {
            project_name: { contains: global_search, mode: 'insensitive' },
          },
        },
        {
          site_data: {
            name: { contains: global_search, mode: 'insensitive' },
          },
        }
      );
    }

    const result = await stockAuditDao.searchStockAudit(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempStockAuditData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempStockAuditData;
  } catch (error) {
    console.log(
      'Error occurred in searchStockAudit StockAudit service : ',
      error
    );
    throw error;
  }
};

export {
  createStockAudit,
  updateStockAudit,
  getAllStockAudits,
  getById,
  deleteStockAudit,
  searchStockAudit,
};
