import siteExpenseDao from '../dao/siteExpense.dao';
import siteDao from '../dao/siteContractor.dao';
import projectDao from '../dao/project.dao';
import {
  createSiteExpenseBody,
  updateSiteExpenseBody,
} from '../interfaces/siteExpense.Interface';

/**
 * Method to Create a New siteExpense
 * @param body
 * @returns
 */
const createSiteExpense = async (body: createSiteExpenseBody) => {
  try {
    const {
      site_id,
      project_id,
      employee_name,
      employee_id,
      employee_phone,
      purpose,
      department,
      designation,
      start_date,
      end_date,
      created_by,
      site_expense_details,
    } = body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getBySiteId(site_id);
      if (!siteExist) {
        result = {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    let siteExpenseExist = null;
    let siteExpenseDetails = null;
    if (project_id && site_id) {
      siteExpenseExist = await siteExpenseDao.getByProjectIdAndSiteId(
        project_id,
        site_id
      );
    }
    if (!siteExpenseExist) {
      siteExpenseDetails = await siteExpenseDao.add(
        site_id,
        project_id,
        employee_name,
        employee_id,
        employee_phone,
        purpose,
        department,
        designation,
        start_date,
        end_date,
        created_by,
        site_expense_details
      );
    } else {
      siteExpenseDetails = await siteExpenseDao.edit(
        site_id,
        project_id,
        employee_name,
        employee_id,
        employee_phone,
        purpose,
        department,
        designation,
        start_date,
        end_date,
        created_by,
        siteExpenseExist?.site_expense_id,
        site_expense_details
      );
    }
    result = { message: 'success', status: true, data: siteExpenseDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in siteExpense service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing siteExpense
 * @param body
 * @returns
 */
const updateSiteExpense = async (body: updateSiteExpenseBody) => {
  try {
    const {
      site_id,
      project_id,
      employee_name,
      employee_id,
      employee_phone,
      purpose,
      department,
      designation,
      start_date,
      end_date,
      updated_by,
      site_expense_id,
      site_expense_details,
    } = body;
    let result = null;

    if (site_id) {
      const siteExist = await siteDao.getBySiteId(site_id);
      if (!siteExist) {
        result = {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        result = {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const siteExpenseExist = await siteExpenseDao.getById(site_expense_id);

    if (siteExpenseExist) {
      const siteExpenseDetails = await siteExpenseDao.edit(
        site_id,
        project_id,
        employee_name,
        employee_id,
        employee_phone,
        purpose,
        department,
        designation,
        start_date,
        end_date,
        updated_by,
        site_expense_id,
        site_expense_details
      );
      result = { message: 'success', status: true, data: siteExpenseDetails };
      return result;
    } else {
      result = {
        message: 'site_expense_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in siteExpense service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get siteExpense By siteExpenseId
 * @param siteExpenseId
 * @returns
 */
const getById = async (siteExpenseId: number) => {
  try {
    let result = null;
    const siteExpenseData = await siteExpenseDao.getById(siteExpenseId);
    if (siteExpenseData) {
      result = { message: 'success', status: true, data: siteExpenseData };
      return result;
    } else {
      result = {
        message: 'site_expense_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById siteExpense service : ', error);
    throw error;
  }
};

/**
 * Method to Get All siteExpense's
 * @returns
 */
const getAllSiteExpense = async () => {
  try {
    const result = await siteExpenseDao.getAll();
    const siteExpenseData = { message: 'success', status: true, data: result };
    return siteExpenseData;
  } catch (error) {
    console.log(
      'Error occurred in getAllSiteExpense siteExpense service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete siteExpense
 * @param siteExpenseId
 */
const deleteSiteExpense = async (siteExpenseId: number) => {
  try {
    const siteExpenseExist = await siteExpenseDao.getById(siteExpenseId);
    if (!siteExpenseExist) {
      const result = {
        message: 'site_expense_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (siteExpenseExist?.site_expense_details?.length > 0) {
      const result = {
        message: 'Unable to delete this.The site_expense_id is mapped on site_expense_details',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await siteExpenseDao.deleteSiteExpense(siteExpenseId);
    if (data) {
      const result = {
        message: 'siteExpense Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this siteExpense',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteSiteExpense siteExpense service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search SiteExpense - Pagination API
 * @returns
 */
const searchSiteExpense = async (body) => {
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

    const filterObj: any = {};

    if (status) {
      filterObj.filterSiteExpense = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (global_search) {
      filterObj.filterSiteExpense = filterObj.filterSiteExpense || {};
      filterObj.filterSiteExpense.OR = filterObj.filterSiteExpense.OR || [];

      filterObj.filterSiteExpense.OR.push(
        {
          employee_name: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          employee_id: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          employee_phone: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          purpose: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          department: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          designation: {
            contains: global_search,
            mode: 'insensitive',
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
          project_data: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );

      filterObj.filterSiteExpense.OR.push({
        OR: [
          {
            site_expense_details: {
              some: {
                description: {
                  contains: global_search,
                  mode: 'insensitive',
                },
              },
            },
          },
          {
            site_expense_details: {
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
        ],
      });
    }

    const result = await siteExpenseDao.searchSiteExpense(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempSiteExpenseData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempSiteExpenseData;
  } catch (error) {
    console.log(
      'Error occurred in searchSiteExpense siteExpense service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get siteExpense By Project Id and Site Id
 * @param project_id
 * @param site_id
 * @returns
 */
const getByProjectIdAndSiteId = async (project_id: number, site_id: number) => {
  try {
    let result = null;
    const siteExpenseData = await siteExpenseDao.getByProjectIdAndSiteId(
      project_id,
      site_id
    );
    if (siteExpenseData) {
      result = { message: 'success', status: true, data: siteExpenseData };
      return result;
    } else {
      result = {
        message: 'site_expense does not exist for this project_id and site_id combination',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectIdAndSiteId siteExpense service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get siteExpense By Project Id and Site Id
 * @param project_id
 * @param site_id
 * @returns
 */
const getSiteExpenseDetailsBySiteExpenseId = async (site_expense_id: number) => {
  try {
    let result = null;
    const status = 'Pending';
    const siteExpenseDetailsData = await siteExpenseDao.getSiteExpenseDeatilsBySiteExpenceId(
      site_expense_id,
      status,
    );
    if (siteExpenseDetailsData) {
      result = { message: 'success', status: true, data: siteExpenseDetailsData };
      return result;
    } else {
      result = {
        message: 'site_expense does not exist for this site_expense_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectIdAndSiteId siteExpense service : ',
      error
    );
    throw error;
  }
};

export {
  createSiteExpense,
  updateSiteExpense,
  getAllSiteExpense,
  getById,
  deleteSiteExpense,
  searchSiteExpense,
  getByProjectIdAndSiteId,
  getSiteExpenseDetailsBySiteExpenseId,
};
