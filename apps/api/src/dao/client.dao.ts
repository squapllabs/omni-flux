import prisma from '../utils/prisma';

const add = async (
  name: string,
  contact_details: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const client = await transaction.client.create({
      data: {
        name,
        contact_details,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
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
    const client = await transaction.client.findFirst({
      where: {
        client_id: Number(clientId),
        is_delete: false,
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
      where: {
        is_delete: false,
      },
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
    const client = await transaction.client.update({
      where: {
        client_id: Number(clientId),
      },
      data: {
        is_delete: true,
      },
    });
    return client;
  } catch (error) {
    console.log('Error occurred in client deleteClient dao', error);
    throw error;
  }
};

const searchClient = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterClient;
    const getdata = await transaction.client.findMany({
      where: {
        is_delete: false,
      },
    });
    if (getdata.length != 0) {
      const client = await transaction.client.findMany({
        where: filter,
        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
      const clientCount = await transaction.client.count({
        where: filter,
      });
      const clientData = {
        count: clientCount,
        data: client,
      };
      return clientData;
    } else {
      return false;
    }
  } catch (error) {
    console.log('Error occurred in client dao : searchClient', error);
    throw error;
  }
};

const getByName = async (name: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const client =
      await transaction.$queryRaw`select * from client c where lower(c."name")=lower(${name})`;
    return client;
  } catch (error) {
    console.log('Error occurred in client getById dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteClient,
  searchClient,
  getByName,
};
