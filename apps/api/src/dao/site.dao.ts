import prisma from '../utils/prisma';

const add = async (
  site_name: string,
  location: string,
  user_id: number,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const site = await transaction.site.create({
      data: {
        site_name,
        location,
        user_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return site;
  } catch (error) {
    console.log('Error occurred in siteDao add', error);
    throw error;
  }
};

const edit = async (
  site_name: string,
  location: string,
  user_id: number,
  updated_by: bigint,
  site_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const site = await transaction.site.update({
      where: {
        site_id: site_id,
      },
      data: {
        site_name,
        location,
        user_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return site;
  } catch (error) {
    console.log('Error occurred in siteDao edit', error);
    throw error;
  }
};

const getById = async (siteId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const site = await transaction.site.findUnique({
      where: {
        site_id: Number(siteId),
      },
    });

    if (site && site.user_id) {
      const user = await transaction.users.findUnique({
        where: {
          user_id: site.user_id,
        },
      });
      site.user = user;
    }

    return site;
  } catch (error) {
    console.log('Error occurred in site getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const site = await transaction.site.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return site;
  } catch (error) {
    console.log('Error occurred in site getAll dao', error);
    throw error;
  }
};

const deleteSite = async (siteId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const site = await transaction.site.delete({
      where: {
        site_id: Number(siteId),
      },
    });
    return site;
  } catch (error) {
    console.log('Error occurred in site deleteSite dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteSite,
};
