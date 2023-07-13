import prisma from '../utils/prisma';

const add = async (
  name: string,
  contact_details: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const client = await transaction.client.create({
      data: {
        name,
        contact_details,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return client;
  } catch (error) {
    console.log('Error occurred in clientDao add', error);
    throw error;
  }
};

const edit = async (
  name: string,
  contact_details: string,
  updated_by: bigint,
  client_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const client = await transaction.client.update({
      where: {
        client_id: client_id,
      },
      data: {
        name,
        contact_details,
        updated_by,
        updated_date: currentDate,
      },
    });
    return client;
  } catch (error) {
    console.log('Error occurred in clientDao edit', error);
    throw error;
  }
};

const getById = async (clientId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const client = await transaction.client.findUnique({
      where: {
        client_id: Number(clientId),
      },
    });
    return client;
  } catch (error) {
    console.log('Error occurred in client getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const client = await transaction.client.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return client;
  } catch (error) {
    console.log('Error occurred in client getAll dao', error);
    throw error;
  }
};

const deleteClient = async (clientId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const client = await transaction.client.delete({
      where: {
        client_id: Number(clientId),
      },
    });
    return client;
  } catch (error) {
    console.log('Error occurred in client deleteClient dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteClient,
};
