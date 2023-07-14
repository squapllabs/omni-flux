import prisma from '../utils/prisma';

const add = async (
  project_name: string,
  description: string,
  user_id: number,
  date_started: Date,
  date_ended: Date,
  status: string,
  budget: number,
  client_id: number,
  document_url: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_date_started = date_started ? new Date(date_started) : null;
    const formatted_date_ended = date_ended ? new Date(date_ended) : null;

    const project = await transaction.project.create({
      data: {
        project_name,
        description,
        user_id,
        date_started: formatted_date_started,
        date_ended: formatted_date_ended,
        status,
        budget,
        client_id,
        document_url,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return project;
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
  budget: number,
  client_id: number,
  document_url: string,
  updated_by: bigint,
  project_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_date_started = date_started ? new Date(date_started) : null;
    const formatted_date_ended = date_ended ? new Date(date_ended) : null;
    const project = await transaction.project.update({
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
        budget,
        client_id,
        document_url,
        updated_by,
        updated_date: currentDate,
      },
    });
    return project;
  } catch (error) {
    console.log('Error occurred in projectDao edit', error);
    throw error;
  }
};

const getById = async (projectId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const project = await transaction.project.findUnique({
      where: {
        project_id: Number(projectId),
      },
    });
    if (project && project?.is_delete === true) {
      return null;
    }
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

const deleteProject = async (projectId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const project = await transaction.project.update({
      where: {
        project_id: Number(projectId),
      },
      data: {
        is_delete: true,
      },
    });
    return project;
  } catch (error) {
    console.log('Error occurred in project deleteProject dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteProject,
};
