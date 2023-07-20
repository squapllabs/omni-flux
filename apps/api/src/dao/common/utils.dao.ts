import db from '../../utils/db';

const customQueryExecutor = async (customQuery, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = customQuery;
    const result = await transaction.query(query, []);
    return result;
  } catch (error) {
    console.log('Error occurred in customQueryExecutor', error);
    throw error;
  }
};

export default {
  customQueryExecutor,
};
