import prisma from '../utils/prisma';
import db from '../utils/db';

const add = async (
  project_name: string,
  description: string,
  user_id: number,
  date_started: Date,
  date_ended: Date,
  status: string,
  estimated_budget: number,
  actual_budget: number,
  code: string,
  project_type: string,
  project_notes: string,
  client_id: number,
  project_documents: JSON,
  created_by: bigint,
  site_configuration,
  approvar_id: number,
  bom_configuration,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_date_started = date_started ? new Date(date_started) : null;
    const formatted_date_ended = date_ended ? new Date(date_ended) : null;

    const result = await transaction
      .$transaction(async (tx) => {
        /* Project - Add */

        const project = await tx.project.create({
          data: {
            project_name,
            description,
            user_id,
            date_started: formatted_date_started,
            date_ended: formatted_date_ended,
            status,
            estimated_budget,
            actual_budget,
            code,
            project_type,
            project_notes,
            client_id,
            project_documents,
            created_by,
            created_date: currentDate,
            updated_date: currentDate,
            is_delete: is_delete,
            approvar_id,
          },
        });

        /*  Project Site - Add */

        const newProjectId = project.project_id;

        const projectSiteDetails = [];
        for (const site of site_configuration) {
          const site_id = site.site_id;
          const status = site.status;
          const is_delete = site.is_delete;
          const estimated_budget = site.estimated_budget;
          const actual_budget = site.actual_budget;
          const approvar_id = site.approvar_id;

          if (is_delete === 'N') {
            const projectSite = await tx.project_site.create({
              data: {
                project_id: newProjectId,
                site_id: site_id,
                status: status,
                estimated_budget: estimated_budget,
                actual_budget: actual_budget,
                created_by,
                created_date: currentDate,
                updated_date: currentDate,
                approvar_id: approvar_id,
              },
            });
            projectSiteDetails.push(projectSite);
          }
        }

        /* BOM Configuration - Add */

        const bomConfigurationDetails = [];
        for (const bom of bom_configuration) {
          const bom_name = bom.bom_name;
          const bom_description = bom.bom_description;
          const bom_type_id = bom.bom_type_id;
          const budget = bom.budget ? bom.budget : 0;
          const is_delete = bom.is_delete;

          if (is_delete === 'N') {
            const bomConfiguration = await tx.bom_configuration.create({
              data: {
                bom_name,
                bom_description,
                bom_type_id,
                project_id: newProjectId,
                budget,
                is_delete: false,
                created_by,
                created_date: currentDate,
                updated_date: currentDate,
              },
            });
            bomConfigurationDetails.push(bomConfiguration);
          }
        }

        const result = {
          project: project,
          project_site: projectSiteDetails,
          bom_configuration: bomConfigurationDetails,
        };

        return result;
      })
      .then((data) => {
        console.log('Successfully Project Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in projectDao add', error);
    throw error;
  }
};

const edit = async (
  project_name: string,
  description: string,
  user_id: number,
  date_started: Date,
  date_ended: Date,
  status: string,
  estimated_budget: number,
  actual_budget: number,
  code: string,
  project_type: string,
  project_notes: string,
  client_id: number,
  project_documents: JSON,
  updated_by: bigint,
  project_id: number,
  site_configuration,
  approvar_id: number,
  bom_configuration,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const result = await transaction
      .$transaction(async (tx) => {
        /* Project - Edit */

        const formatted_date_started = date_started
          ? new Date(date_started)
          : null;
        const formatted_date_ended = date_ended ? new Date(date_ended) : null;
        const project = await tx.project.update({
          where: {
            project_id: project_id,
          },
          data: {
            project_name,
            description,
            user_id,
            date_started: formatted_date_started,
            date_ended: formatted_date_ended,
            status,
            estimated_budget,
            actual_budget,
            code,
            project_type,
            project_notes,
            client_id,
            project_documents,
            updated_by,
            updated_date: currentDate,
            approvar_id,
          },
        });

        /* Project Site - Edit */

        const projectSiteDetails = [];

        for (const site of site_configuration) {
          const site_id = site.site_id;
          const status = site.status;
          const is_delete = site.is_delete;
          const project_site_id = site.project_site_id;
          const estimated_budget = site.estimated_budget;
          const actual_budget = site.actual_budget;
          const approvar_id = site.approvar_id;

          if (project_site_id) {
            if (is_delete === 'Y') {
              await tx.project_site.delete({
                where: { project_site_id: project_site_id },
              });
            } else {
              const projectSite = await tx.project_site.update({
                where: { project_site_id: project_site_id },
                data: {
                  project_id: project_id,
                  site_id: site_id,
                  status: status,
                  estimated_budget: estimated_budget,
                  actual_budget: actual_budget,
                  updated_by,
                  updated_date: currentDate,
                  approvar_id: approvar_id,
                },
              });
              projectSiteDetails.push(projectSite);
            }
          } else {
            if (is_delete === 'N') {
              const projectSite = await tx.project_site.create({
                data: {
                  project_id: project_id,
                  site_id: site_id,
                  status: status,
                  estimated_budget: estimated_budget,
                  actual_budget: actual_budget,
                  created_by: updated_by,
                  created_date: currentDate,
                  updated_date: currentDate,
                  approvar_id: approvar_id,
                },
              });
              projectSiteDetails.push(projectSite);
            }
          }
        }

        /* BOM - Configuration Edit */

        const bomConfigurationDetails = [];
        for (const bom of bom_configuration) {
          const bom_name = bom.bom_name;
          const bom_description = bom.bom_description;
          const bom_type_id = bom.bom_type_id;
          const budget = bom.budget ? bom.budget : 0;
          const is_delete = bom.is_delete;
          const bom_configuration_id = bom.bom_configuration_id;

          if (bom_configuration_id) {
            if (is_delete === 'Y') {
              await tx.bom_configuration.update({
                where: { bom_configuration_id: bom_configuration_id },
                data: { is_delete: true },
              });
            } else {
              const bomConfiguration = await tx.bom_configuration.update({
                where: { bom_configuration_id: bom_configuration_id },
                data: {
                  bom_name,
                  bom_description,
                  bom_type_id,
                  project_id,
                  budget,
                  updated_by,
                  updated_date: currentDate,
                },
              });
              bomConfigurationDetails.push(bomConfiguration);
            }
          } else {
            if (is_delete === 'N') {
              const bomConfiguration = await tx.bom_configuration.create({
                data: {
                  bom_name,
                  bom_description,
                  bom_type_id,
                  project_id,
                  budget,
                  is_delete: false,
                  created_by: updated_by,
                  created_date: currentDate,
                  updated_date: currentDate,
                },
              });
              bomConfigurationDetails.push(bomConfiguration);
            }
          }
        }

        const result = {
          project: project,
          project_site: projectSiteDetails,
          bom_configuration: bomConfigurationDetails,
        };

        return result;
      })
      .then((data) => {
        console.log('Successfully Project Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in projectDao edit', error);
    throw error;
  }
};

const getById = async (projectId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const project = await transaction.project.findFirst({
      where: {
        project_id: Number(projectId),
        is_delete: false,
      },
      include: {
        project_site: {
          include: {
            site_details: true,
          },
        },
        bom_configuration: {
          where: {
            is_delete: false,
          },
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
          orderBy: [{ updated_date: 'desc' }],
        },
        user: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        client: {
          select: {
            name: true,
          },
        },
        approvar_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
      },
    });
    return project;
  } catch (error) {
    console.log('Error occurred in project getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const project = await transaction.project.findMany({
      where: {
        is_delete: false,
      },
      include: {
        project_site: {
          include: {
            site_details: true,
          },
        },
        bom_configuration: {
          where: {
            is_delete: false,
          },
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
          orderBy: [{ updated_date: 'desc' }],
        },
        user: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        client: {
          select: {
            name: true,
          },
        },
        approvar_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return project;
  } catch (error) {
    console.log('Error occurred in project getAll dao', error);
    throw error;
  }
};

const getAllDashboard = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    let query = `select
    status.total_projects,
    status.active_projects,
    status.inactive_projects,
    status.inprogress_projects,
    status.completed_projects,
    status.not_started_projects,
    vendor_list.vendor_count,
    project_list.top_projects
  from
    (
    select
      count(*) as total_projects,
      count(p.is_delete)filter (
    where
      is_delete = false)as active_projects,
      count(p.is_delete)filter (
    where
      is_delete = true)as inactive_projects,
      count(status)filter (
    where
      status = 'Inprogress')as inprogress_projects,
      count(status)filter (
    where
      status = 'Completed')as completed_projects,
      count(status)filter (
    where
      status = 'Not Started')as not_started_projects
    from
      project p)status
  join (
    select
      count(vendor_id) as vendor_count
    from
      vendor v )vendor_list on
    true
  join (
    select
      jsonb_agg(top_five.top_five_projects) as top_projects
    from
      (
      select
        jsonb_build_object('project_total_days',
        (p.date_ended - p.date_started),
        'days_completed',
        (current_date - date_started ),
        'project_name',
        p.project_name,
       'total_budget',
        p.actual_budget) as top_five_projects
      from
        project p
      order by
        p.actual_budget desc
      limit 3) top_five)project_list on
    true`;
    let result = await transaction.oneOrNone(query, []);
    return result;
  } catch (error) {
    console.log('Error occurred in getAllDashboard dao', error);
    throw error;
  }
};

const deleteProject = async (projectId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const currentDate = new Date();
    const project = await transaction.project.update({
      where: {
        project_id: Number(projectId),
      },
      data: {
        is_delete: true,
        updated_date: currentDate,
      },
    });
    return project;
  } catch (error) {
    console.log('Error occurred in project deleteProject dao', error);
    throw error;
  }
};

const getByClientId = async (clientId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const project = await transaction.project.findFirst({
      where: {
        client_id: Number(clientId),
        is_delete: false,
      },
    });
    return project;
  } catch (error) {
    console.log('Error occurred in project getByClientId dao', error);
    throw error;
  }
};

const getByCode = async (code: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const project = await transaction.project.findFirst({
      where: {
        code: code,
      },
    });
    return project;
  } catch (error) {
    console.log('Error occurred in project getByCode dao', error);
    throw error;
  }
};

const searchProject = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterProject;
    const project = await transaction.project.findMany({
      where: filter,
      include: {
        project_site: {
          include: {
            site_details: true,
          },
        },
        user: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        client: {
          select: {
            name: true,
          },
        },
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
    const projectCount = await transaction.project.count({
      where: filter,
    });

    const projectData = {
      count: projectCount,
      data: project,
    };
    return projectData;
  } catch (error) {
    console.log('Error occurred in project dao : searchProject ', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteProject,
  getByClientId,
  getByCode,
  searchProject,
  getAllDashboard,
};
