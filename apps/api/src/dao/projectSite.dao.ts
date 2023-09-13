import prisma from '../utils/prisma';

const add = async (
  project_id: number,
  site_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.create({
      data: {
        project_id,
        site_id,
      },
    });
    return projectSite;
  } catch (error) {
    console.log('Error occurred in projectSiteDao add', error);
    throw error;
  }
};

const edit = async (
  project_id: number,
  site_id: number,
  project_site_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.update({
      where: {
        project_site_id: project_site_id,
      },
      data: {
        project_id,
        site_id,
      },
    });
    return projectSite;
  } catch (error) {
    console.log('Error occurred in projectSiteDao edit', error);
    throw error;
  }
};

const getByProjectIdAndSiteId = async (
  project_id: number,
  site_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.findFirst({
      where: {
        project_id: project_id,
        site_id: site_id,
      },
    });
    return projectSite;
  } catch (error) {
    console.log(
      'Error occurred in projectSiteDao getByProjectIdAndSiteId',
      error
    );
    throw error;
  }
};

const getByProjectId = async (project_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.findMany({
      where: {
        project_id: Number(project_id),
      },
      include: {
        site_details: true,
        project_details: true,
      },
    });
    return projectSite;
  } catch (error) {
    console.log('Error occurred in projectSiteDao getByProjectId', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getByProjectIdAndSiteId,
  getByProjectId,
};
