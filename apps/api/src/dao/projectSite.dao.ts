import prisma from '../utils/prisma';

const add = async (
  project_id: number,
  site_id: number,
  status: string,
  estimated_budget: number,
  actual_budget: number,
  created_by: number,
  approvar_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.create({
      data: {
        project_id,
        site_id,
        status,
        estimated_budget,
        actual_budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        approvar_id,
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
  status: string,
  estimated_budget: number,
  actual_budget: number,
  updated_by: number,
  approvar_id: number,
  project_site_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.update({
      where: {
        project_site_id: project_site_id,
      },
      data: {
        project_id,
        site_id,
        status,
        estimated_budget,
        actual_budget,
        updated_by,
        updated_date: currentDate,
        approvar_id,
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
      include: {
        approvar_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        project_details: true,
        site_details: true,
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
        approvar_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
      },
    });
    return projectSite;
  } catch (error) {
    console.log('Error occurred in projectSiteDao getByProjectId', error);
    throw error;
  }
};

const getByProjectSiteId = async (
  project_site_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.findFirst({
      where: {
        project_site_id: Number(project_site_id),
      },
      include: {
        site_details: true,
        project_details: true,
        approvar_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
      },
    });
    return projectSite;
  } catch (error) {
    console.log('Error occurred in projectSiteDao getByProjectSiteId', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectSite = await transaction.project_site.findMany({
      include: {
        site_details: true,
        project_details: true,
        approvar_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
      },
      orderBy: { updated_date: 'desc' },
    });
    return projectSite;
  } catch (error) {
    console.log('Error occurred in projectSiteDao getAll', error);
    throw error;
  }
};

const searchProjectSite = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterProjectSite;
    const projectSite = await transaction.project_site.findMany({
      where: filter,
      include: {
        site_details: true,
        project_details: true,
        approvar_data: {
          select: {
            first_name: true,
            last_name: true,
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
    const projectSiteCount = await transaction.project_site.count({
      where: filter,
    });

    const projectSiteData = {
      count: projectSiteCount,
      data: projectSite,
    };
    return projectSiteData;
  } catch (error) {
    console.log('Error occurred in project dao : searchProjectSite ', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getByProjectIdAndSiteId,
  getByProjectId,
  getByProjectSiteId,
  getAll,
  searchProjectSite,
};
