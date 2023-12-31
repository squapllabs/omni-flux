import projectWorkbreakDownDao from '../dao/projectWorkbreakDown.dao';
import {
  createProjectWorkbreakDownBody,
  updateProjectWorkbreakDownBody,
} from '../interfaces/projectWorkbreakDown.Interface';
import uomDao from '../dao/uom.dao';
import projectDao from '../dao/project.dao';
import siteDao from '../dao/siteContractor.dao';

/**
 * Method to Create a New ProjectWorkbreakDown
 * @param body
 * @returns
 */
const createProjectWorkbreakDown = async (
  body: createProjectWorkbreakDownBody
) => {
  try {
    const {
      project_workbreak_down_name,
      project_workbreak_down_description,
      project_workbreak_down_code,
      parent_project_workbreak_down_id,
      rate,
      uom_id,
      project_workbreak_down_type,
      project_id,
      site_id,
      created_by,
    } = body;

    let result = null;

    if (parent_project_workbreak_down_id) {
      const parentProjectWorkbreakDownExist =
        await projectWorkbreakDownDao.getById(parent_project_workbreak_down_id);
      if (!parentProjectWorkbreakDownExist) {
        result = {
          massage: 'parent_project_workbreak_down_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (uom_id) {
      const uomExist = await uomDao.getById(uom_id);
      if (!uomExist) {
        result = {
          massage: 'uom_id does not exist',
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
          massage: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (site_id) {
      const siteExist = await siteDao.getById(site_id);
      if (!siteExist) {
        result = {
          massage: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const projectWorkbreakDownDetails = await projectWorkbreakDownDao.add(
      project_workbreak_down_name,
      project_workbreak_down_description,
      project_workbreak_down_code,
      parent_project_workbreak_down_id,
      rate,
      uom_id,
      project_workbreak_down_type,
      project_id,
      site_id,
      created_by
    );
    result = {
      massage: 'success',
      status: true,
      data: projectWorkbreakDownDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in projectWorkbreakDown service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing ProjectWorkbreakDown
 * @param body
 * @returns
 */
const updateProjectWorkbreakDown = async (
  body: updateProjectWorkbreakDownBody
) => {
  try {
    const {
      project_workbreak_down_name,
      project_workbreak_down_description,
      project_workbreak_down_code,
      parent_project_workbreak_down_id,
      rate,
      uom_id,
      project_workbreak_down_type,
      project_id,
      site_id,
      updated_by,
      project_workbreak_down_id,
    } = body;
    let result = null;

    const projectWorkbreakDownExist = await projectWorkbreakDownDao.getById(
      project_workbreak_down_id
    );

    if (parent_project_workbreak_down_id) {
      const parentProjectWorkbreakDownExist =
        await projectWorkbreakDownDao.getById(parent_project_workbreak_down_id);
      if (!parentProjectWorkbreakDownExist) {
        result = {
          massage: 'parent_project_workbreak_down_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (uom_id) {
      const uomExist = await uomDao.getById(uom_id);
      if (!uomExist) {
        result = {
          massage: 'uom_id does not exist',
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
          massage: 'project_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (site_id) {
      const siteExist = await siteDao.getById(site_id);
      if (!siteExist) {
        result = {
          massage: 'site_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (projectWorkbreakDownExist) {
      const projectWorkbreakDownDetails = await projectWorkbreakDownDao.edit(
        project_workbreak_down_name,
        project_workbreak_down_description,
        project_workbreak_down_code,
        parent_project_workbreak_down_id,
        rate,
        uom_id,
        project_workbreak_down_type,
        project_id,
        site_id,
        updated_by,
        project_workbreak_down_id
      );
      result = {
        massage: 'success',
        status: true,
        data: projectWorkbreakDownDetails,
      };
      return result;
    } else {
      result = {
        message: 'project_workbreak_down_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in projectWorkbreakDown service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get ProjectWorkbreakDown By ProjectWorkbreakDownId
 * @param projectWorkbreakDownId
 * @returns
 */
const getById = async (projectWorkbreakDownId: number) => {
  try {
    let result = null;
    const projectWorkbreakDownData = await projectWorkbreakDownDao.getById(
      projectWorkbreakDownId
    );
    if (projectWorkbreakDownData) {
      result = {
        massage: 'success',
        status: true,
        data: projectWorkbreakDownData,
      };
      return result;
    } else {
      result = {
        message: 'project_workbreak_down_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getById projectWorkbreakDown service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All ProjectWorkbreakDown's
 * @returns
 */
const getAllProjectWorkbreakDown = async () => {
  try {
    const result = await projectWorkbreakDownDao.getAll();
    const projectWorkbreakDownData = {
      massage: 'success',
      status: true,
      data: result,
    };
    return projectWorkbreakDownData;
  } catch (error) {
    console.log(
      'Error occurred in getAllProjectWorkbreakDown projectWorkbreakDown service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete projectWorkbreakDown
 * @param projectWorkbreakDownId
 */
const deleteProjectWorkbreakDown = async (projectWorkbreakDownId: number) => {
  try {
    const projectWorkbreakDownExist = await projectWorkbreakDownDao.getById(
      projectWorkbreakDownId
    );
    if (!projectWorkbreakDownExist) {
      const result = {
        message: 'project_workbreak_down_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await projectWorkbreakDownDao.deleteProjectWorkbreakDown(
      projectWorkbreakDownId
    );
    if (data) {
      const result = {
        message: 'ProjectWorkbreakDown Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this projectWorkbreakDown',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteProjectWorkbreakDown projectWorkbreakDown service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get ProjectWorkbreakDown By ProjectWorkbreakDownCode
 * @param body
 * @returns
 */
const getByCode = async (body) => {
  try {
    let result = null;
    const { code, project_id, site_id } = body;

    const projectWorkbreakDownDataByType =
      await projectWorkbreakDownDao.getByCode(code, project_id, site_id);

    /*     let projectWorkbreakDownDataByProjectId = null;
    let projectWorkbreakDownDataBySite = null;
    let projectWorkbreakDownDataByProjectSite = null;

    if (project_id) {
      projectWorkbreakDownDataByProjectId =
        await projectWorkbreakDownDao.getByProjectId(project_id);
    }

    if (site_id) {
      projectWorkbreakDownDataBySite =
        await projectWorkbreakDownDao.getBySiteId(site_id);
    }

    if (project_id && site_id) {
      projectWorkbreakDownDataByProjectSite =
        await projectWorkbreakDownDao.getByProjectIdAndSiteId(
          project_id,
          site_id
        );
    }
 */
    result = {
      message: 'success',
      status: true,
      data: projectWorkbreakDownDataByType,
      /*  data: {
        data_by_type: projectWorkbreakDownDataByType,
        data_by_project_id: projectWorkbreakDownDataByProjectId,
        data_by_site_id: projectWorkbreakDownDataBySite,
        data_by_project_site: projectWorkbreakDownDataByProjectSite,
      }, */
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in getById projectWorkbreakDown service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search ProjectWorkbreakDown - Pagination API
 * @returns
 */
const searchProjectWorkbreakDown = async (body) => {
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
    const parent_project_workbreak_down_id = body.parent_id;

    const filterObj: any = {};

    if (status) {
      filterObj.filterProjectWorkbreakDown = {
        is_delete: status === 'AC' ? false : true,
        project_workbreak_down_type: 'DEFAULT',
      };
    }

    if (parent_project_workbreak_down_id) {
      filterObj.filterProjectWorkbreakDown =
        filterObj.filterProjectWorkbreakDown || {};
      filterObj.filterProjectWorkbreakDown.AND =
        filterObj.filterProjectWorkbreakDown.AND || [];

      if (parent_project_workbreak_down_id) {
        filterObj.filterProjectWorkbreakDown.AND.push({
          parent_project_workbreak_down_id: Number(
            parent_project_workbreak_down_id
          ),
        });
      }
    }

    if (global_search) {
      filterObj.filterProjectWorkbreakDown =
        filterObj.filterProjectWorkbreakDown || {};
      filterObj.filterProjectWorkbreakDown.OR =
        filterObj.filterProjectWorkbreakDown.OR || [];

      filterObj.filterProjectWorkbreakDown.OR.push(
        {
          project_workbreak_down_name: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          project_workbreak_down_description: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          project_workbreak_down_code: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          project_workbreak_down_type: {
            contains: global_search,
            mode: 'insensitive',
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
          uom_details: {
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

    const result = await projectWorkbreakDownDao.searchProjectWorkbreakDown(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempProjectWorkbreakDownData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempProjectWorkbreakDownData;
  } catch (error) {
    console.log(
      'Error occurred in searchProjectWorkbreakDown projectWorkbreakDown service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All Parent ProjectWorkbreakDown's
 * @returns
 */
const getAllParentProjectWorkbreakDown = async () => {
  try {
    const result =
      await projectWorkbreakDownDao.getAllParentProjectWorkbreakDownData();
    const projectWorkbreakDownData = {
      massage: 'success',
      status: true,
      data: result,
    };
    return projectWorkbreakDownData;
  } catch (error) {
    console.log(
      'Error occurred in getAllParentProjectWorkbreakDown projectWorkbreakDown service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to check Duplicate project_workbreak_down_code
 * @param project_workbreak_down_code
 * @returns
 */
const checkDuplicateCode = async (project_workbreak_down_code: string) => {
  try {
    let result = null;
    const projectWorkbreakDownData =
      await projectWorkbreakDownDao.checkDuplicateCode(
        project_workbreak_down_code
      );
    if (projectWorkbreakDownData) {
      result = {
        massage: 'The project_workbreak_down_code is already exist',
        status: true,
        is_exist: true,
        data: projectWorkbreakDownData,
      };
      return result;
    } else {
      result = {
        massage: 'The project_workbreak_down_code does not exist',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getById checkDuplicateCode service : ',
      error
    );
    throw error;
  }
};

export {
  createProjectWorkbreakDown,
  updateProjectWorkbreakDown,
  getAllProjectWorkbreakDown,
  getById,
  deleteProjectWorkbreakDown,
  getByCode,
  searchProjectWorkbreakDown,
  getAllParentProjectWorkbreakDown,
  checkDuplicateCode,
};
