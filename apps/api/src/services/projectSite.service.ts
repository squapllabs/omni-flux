import projectDao from '../dao/project.dao';
import projectSiteDao from '../dao/projectSite.dao';

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

export { getByProjectId };
