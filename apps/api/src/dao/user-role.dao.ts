import db from '../utils/db';

const add = async (
  role_id: BigInteger,
  user_id: BigInteger,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `INSERT INTO user_role(role_id,user_id) 
          values ($1,$2) RETURNING *`;
    const result = await transaction.query(query, [role_id, user_id]);
    return result.rows;
  } catch (error) {
    console.log('Error occurred in userDao add', error);
    throw error;
  }
};

export default { add };
