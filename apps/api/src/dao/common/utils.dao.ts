import prisma from '../../utils/prisma';

const customQueryExecutor = async (customQuery, connectionObj = null) => {
  try {
    console.log(customQuery);

    const transaction = connectionObj !== null ? connectionObj : prisma;
    const result = await transaction.$queryRaw(customQuery);
    return result;
  } catch (error) {
    console.log('Error occurred in customQueryExecutor', error);
    throw error;
  }
};

export default {
  customQueryExecutor,
};
