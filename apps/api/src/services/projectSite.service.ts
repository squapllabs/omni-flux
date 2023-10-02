import projectDao from '../dao/project.dao';
import projectSiteDao from '../dao/projectSite.dao';
import siteContractorDao from '../dao/siteContractor.dao';
import { projectSiteBody } from '../interfaces/projectSite.interface';

/**
 * Method to get all sites which are related to the project_id
 * @param project_id
 * @returns
 */
const getByProjectId = async (project_id: number) => {
  try {
    let result = null;
    const projectExist = await projectDao.getById(project_id);
    if (!projectExist) {
      return {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
    }

    const projectSiteData = await projectSiteDao.getByProjectId(project_id);
    if (projectSiteData.length > 0) {
      result = { message: 'success', status: true, data: projectSiteData };
      return result;
    } else {
      result = {
        message: 'No data found related to this project_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById projectSite service : ', error);
    throw error;
  }
};

/**
 * Method to Create a New ProjectSite
 * @param body
 * @returns
 */
const createProjectSite = async (body: projectSiteBody) => {
  try {
    const {
      project_id,
      site_id,
      status,
      estimated_budget,
      actual_budget,
      created_by,
      approvar_id,
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

    const projectSiteDetails = await projectSiteDao.add(
      project_id,
      site_id,
      status,
      Number(estimated_budget),
      Number(actual_budget),
      created_by,
      approvar_id
    );
    const result = {
      message: 'success',
      status: true,
      data: projectSiteDetails,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in projectSite service createProjectSite: ',
      error
    );
    throw error;
  }
};

/**
 * Method to Update an Existing ProjectSite
 * @param body
 * @returns
 */

const updateProjectSite = async (body: projectSiteBody) => {
  try {
    const {
      project_id,
      site_id,
      status,
      estimated_budget,
      actual_budget,
      approvar_id,
      updated_by,
      project_site_id,
    } = body;
    let result = null;
    const projectSiteExist = await projectSiteDao.getByProjectSiteId(
      project_site_id
    );
    if (!projectSiteExist) {
      result = {
        message: 'project_site_id does not exist',
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

    const projectSiteDetails = await projectSiteDao.edit(
      project_id,
      site_id,
      status,
      Number(estimated_budget),
      Number(actual_budget),
      approvar_id,
      updated_by,
      project_site_id
    );
    result = { message: 'success', status: true, data: projectSiteDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in projectSite service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get ProjectSite By ProjectSiteId
 * @param projectSiteId
 * @returns
 */
const getByProjectSiteId = async (projectSiteId: number) => {
  try {
    let result = null;
    const projectSiteData = await projectSiteDao.getByProjectSiteId(
      projectSiteId
    );
    if (projectSiteData) {
      result = { message: 'success', status: true, data: projectSiteData };
      return result;
    } else {
      result = {
        message: 'project_site_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById projectSite service : ', error);
    throw error;
  }
};

/**
 * Method to Get All ProjectSites
 * @returns
 */
const getAllProjectSites = async () => {
  try {
    const result = await projectSiteDao.getAll();
    const projectSiteData = { message: 'success', status: true, data: result };
    return projectSiteData;
  } catch (error) {
    console.log(
      'Error occurred in getAllProjectSites projectSite service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search ProjectSite - Pagination API
 * @returns
 */
const searchProjectSite = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    /* const status = body.status; */

    const filterObj: any = {};

    /*   if (status) {
      filterObj.filterProjectSite = {
        is_delete: status === 'AC' ? false : true,
      };
    } */

    if (global_search) {
      filterObj.filterProjectSite = filterObj.filterProjectSite || {};
      filterObj.filterProjectSite.OR = filterObj.filterProjectSite.OR || [];

      filterObj.filterProjectSite.OR.push(
        {
          approvar_data: {
            first_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          approvar_data: {
            last_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          site_details: {
            name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          project_details: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await projectSiteDao.searchProjectSite(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempProjectSiteData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempProjectSiteData;
  } catch (error) {
    console.log(
      'Error occurred in searchProjectSite ProjectSite service : ',
      error
    );
    throw error;
  }
};

export {
  getByProjectId,
  createProjectSite,
  updateProjectSite,
  getByProjectSiteId,
  getAllProjectSites,
  searchProjectSite,
};
