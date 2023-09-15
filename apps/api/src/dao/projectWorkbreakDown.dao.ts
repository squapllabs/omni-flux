import prisma from '../utils/prisma';

const add = async (
  project_workbreak_down_name: string,
  project_workbreak_down_description: string,
  project_workbreak_down_code: string,
  parent_project_workbreak_down_id: number,
  rate: number,
  uom_id: number,
  project_workbreak_down_type: string,
  project_id: number,
  site_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.create({
        data: {
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
          is_delete: is_delete,
          created_date: currentDate,
          updated_date: currentDate,
        },
      });
    return projectWorkBreakDown;
  } catch (error) {
    console.log('Error occurred in projectWorkBreakDownDao add', error);
    throw error;
  }
};

const edit = async (
  project_workbreak_down_name: string,
  project_workbreak_down_description: string,
  project_workbreak_down_code: string,
  parent_project_workbreak_down_id: number,
  rate: number,
  uom_id: number,
  project_workbreak_down_type: string,
  project_id: number,
  site_id: number,
  updated_by: number,
  project_workbreak_down_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.update({
        where: {
          project_workbreak_down_id: project_workbreak_down_id,
        },
        data: {
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
          updated_date: currentDate,
        },
      });
    return projectWorkBreakDown;
  } catch (error) {
    console.log('Error occurred in projectWorkBreakDownDao edit', error);
    throw error;
  }
};

const getById = async (
  projectWorkBreakDownId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findFirst({
        where: {
          project_workbreak_down_id: Number(projectWorkBreakDownId),
          is_delete: false,
        },
        include: {
          parent_project_workbreak_down: true,
          child_project_workbreak_down: true,
          uom_details: {
            select: {
              name: true,
            },
          },
          project_details: {
            select: {
              project_name: true,
            },
          },
          site_details: {
            select: {
              name: true,
            },
          },
        },
      });

    return projectWorkBreakDown;
  } catch (error) {
    console.log('Error occurred in projectWorkBreakDown getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findMany({
        where: {
          is_delete: false,
          project_workbreak_down_type: 'DEFAULT',
        },
        include: {
          parent_project_workbreak_down: true,
          child_project_workbreak_down: true,
          uom_details: {
            select: {
              name: true,
            },
          },
          project_details: {
            select: {
              project_name: true,
            },
          },
          site_details: {
            select: {
              name: true,
            },
          },
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
    return projectWorkBreakDown;
  } catch (error) {
    console.log('Error occurred in projectWorkBreakDown getAll dao', error);
    throw error;
  }
};

const deleteProjectWorkbreakDown = async (
  projectWorkBreakDownId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.update({
        where: {
          project_workbreak_down_id: Number(projectWorkBreakDownId),
        },
        data: { is_delete: true },
      });
    return projectWorkBreakDown;
  } catch (error) {
    console.log(
      'Error occurred in projectWorkBreakDown deleteProjectWorkbreakDown dao',
      error
    );
    throw error;
  }
};

const getByCode = async (
  type: string,
  projectId: number | null,
  siteId: number | null,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;

    let projectWorkBreakDownQuery;

    if (siteId !== null && projectId !== null) {
      /* Execute query with code, project_id, and site_id conditions */
      projectWorkBreakDownQuery =
        await transaction.project_workbreak_down.findMany({
          where: {
            is_delete: false,
            OR: [
              {
                parent_project_workbreak_down_id: {
                  equals: (
                    await transaction.project_workbreak_down.findFirst({
                      where: {
                        project_workbreak_down_code: type,
                      },
                    })
                  )?.project_workbreak_down_id,
                },
              },
              {
                AND: [
                  {
                    OR: [
                      {
                        project_id: {
                          equals: projectId,
                        },
                      },
                      {
                        AND: [
                          {
                            project_id: {
                              equals: projectId,
                            },
                          },
                          {
                            site_id: {
                              equals: siteId,
                            },
                          },
                        ],
                      },
                    ],
                  },
                ],
              },
            ],
          },

          select: {
            project_workbreak_down_name: true,
          },
          orderBy: {
            project_workbreak_down_name: 'asc',
          },
        });
    } else if (projectId !== null) {
      /* Execute query with code and project_id conditions */
      projectWorkBreakDownQuery =
        await transaction.project_workbreak_down.findMany({
          where: {
            is_delete: false,
            OR: [
              {
                parent_project_workbreak_down_id: {
                  equals: (
                    await transaction.project_workbreak_down.findFirst({
                      where: {
                        project_workbreak_down_code: type,
                      },
                    })
                  )?.project_workbreak_down_id,
                },
              },
              {
                project_id: {
                  equals: projectId,
                },
              },
            ],
          },

          select: {
            project_workbreak_down_name: true,
          },
          orderBy: {
            project_workbreak_down_name: 'asc',
          },
        });
    } else {
      /*  Execute query with code condition only */
      projectWorkBreakDownQuery =
        await transaction.project_workbreak_down.findMany({
          where: {
            is_delete: false,
            parent_project_workbreak_down_id: {
              equals: (
                await transaction.project_workbreak_down.findFirst({
                  where: {
                    project_workbreak_down_code: type,
                  },
                })
              )?.project_workbreak_down_id,
            },
          },

          select: {
            project_workbreak_down_name: true,
          },
          orderBy: {
            project_workbreak_down_name: 'asc',
          },
        });
    }

    return projectWorkBreakDownQuery;
  } catch (error) {
    console.log('Error occurred in projectWorkBreakDown getByCode dao', error);
    throw error;
  }
};

const getByProjectId = async (project_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findMany({
        where: {
          project_id: Number(project_id),
          is_delete: false,
        },
      });

    return projectWorkBreakDown;
  } catch (error) {
    console.log(
      'Error occurred in projectWorkBreakDown getByProjectId dao',
      error
    );
    throw error;
  }
};

const getBySiteId = async (site_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findMany({
        where: {
          site_id: Number(site_id),
          is_delete: false,
        },
      });

    return projectWorkBreakDown;
  } catch (error) {
    console.log(
      'Error occurred in projectWorkBreakDown getBySiteId dao',
      error
    );
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
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findMany({
        where: {
          project_id: Number(project_id),
          site_id: Number(site_id),
          is_delete: false,
        },
      });

    return projectWorkBreakDown;
  } catch (error) {
    console.log(
      'Error occurred in projectWorkBreakDown getByProjectIdAndSiteId dao',
      error
    );
    throw error;
  }
};

const searchProjectWorkbreakDown = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterProjectWorkbreakDown;
    const searchProjectWorkbreakDown =
      await transaction.project_workbreak_down.findMany({
        where: filter,
        include: {
          parent_project_workbreak_down: true,
          child_project_workbreak_down: true,
          uom_details: {
            select: {
              name: true,
            },
          },
          project_details: {
            select: {
              project_name: true,
            },
          },
          site_details: {
            select: {
              name: true,
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

    const searchProjectWorkbreakDownCount =
      await transaction.project_workbreak_down.count({
        where: filter,
      });
    const searchProjectWorkbreakDownData = {
      count: searchProjectWorkbreakDownCount,
      data: searchProjectWorkbreakDown,
    };
    return searchProjectWorkbreakDownData;
  } catch (error) {
    console.log(
      'Error occurred in searchProjectWorkbreakDown dao : searchProjectWorkbreakDown ',
      error
    );
    throw error;
  }
};

const getAllParentProjectWorkbreakDownData = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findMany({
        where: {
          parent_project_workbreak_down_id: null,
          is_delete: false,
        },
        include: {
          parent_project_workbreak_down: true,
          child_project_workbreak_down: true,
          uom_details: {
            select: {
              name: true,
            },
          },
          project_details: {
            select: {
              project_name: true,
            },
          },
          site_details: {
            select: {
              name: true,
            },
          },
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
    return projectWorkBreakDown;
  } catch (error) {
    console.log(
      'Error occurred in projectWorkBreakDown getAllParentProjectWorkbreakDownData dao',
      error
    );
    throw error;
  }
};

const checkDuplicateCode = async (
  projectWorkbreakDownCode: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findFirst({
        where: {
          project_workbreak_down_code: projectWorkbreakDownCode,
          is_delete: false,
        },
      });

    return projectWorkBreakDown;
  } catch (error) {
    console.log(
      'Error occurred in projectWorkBreakDown checkDuplicateCode dao',
      error
    );
    throw error;
  }
};

const getByUomId = async (uomId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectWorkBreakDown =
      await transaction.project_workbreak_down.findFirst({
        where: {
          uom_id: Number(uomId),
        },
      });

    return projectWorkBreakDown;
  } catch (error) {
    console.log('Error occurred in projectWorkBreakDown getByUomId dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteProjectWorkbreakDown,
  getByCode,
  getByProjectId,
  getBySiteId,
  getByProjectIdAndSiteId,
  searchProjectWorkbreakDown,
  getAllParentProjectWorkbreakDownData,
  checkDuplicateCode,
  getByUomId,
};
