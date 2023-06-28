import db from '../utils/db';

const add = async (
  center_id: BigInteger,
  user_name: string,
  user_password: string,
  mobile_number: string,
  email_id: string,
  first_name: string,
  last_name: string,
  profile_img_url: string,
  gender: string,
  dob: Date,
  status: string,
  address: string,
  created_by: BigInteger,
  updated_by: BigInteger,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const currentDate = new Date();
    const query = `INSERT INTO users(center_id,user_name,user_password,mobile_number,
        email_id,first_name,last_name,profile_img_url,gender,dob,status,address,created_by,
        created_date,updated_by,updated_date) 
          values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16) RETURNING *`;
    const result = await transaction.one(query, [
      center_id,
      user_name,
      user_password,
      mobile_number,
      email_id,
      first_name,
      last_name,
      profile_img_url,
      gender,
      dob,
      status,
      address,
      created_by,
      currentDate,
      updated_by,
      currentDate,
    ]);
    return result;
  } catch (error) {
    console.log('Error occurred in userDao add', error);
    throw error;
  }
};

const getById = async (userId: bigint) => {
  try {
    let result = null;
    const query = `select * from users where user_id=$1`;
    result = await db.oneOrNone(query, [userId]);
    return result;
  } catch (error) {
    console.log('Error occurred in getById', error);
    throw error;
  }
};

const getByEmailId = async (emailId: string) => {
  try {
    let result = null;
    const query = `select * from users where email_id=$1`;
    result = await db.oneOrNone(query, [emailId]);
    return result;
  } catch (error) {
    console.log('Error occurred in getById', error);
    throw error;
  }
};

const getByUserName = async (user_name: string) => {
  try {
    let result = null;
    const query = `select * from users where user_name=$1`;
    result = await db.oneOrNone(query, [user_name]);
    return result;
  } catch (error) {
    console.log('Error occurred in getByUserName', error);
    throw error;
  }
};

const userLogin = async (email: string, userpass: string) => {
  try {
    let result = null;
    const query = `select * from users where email=$1 and userpass=$2`;
    result = await db.oneOrNone(query, [email, userpass]);
    return result;
  } catch (error) {
    console.log('Error occurred in getById', error);
    throw error;
  }
};

const getAllUserData = async () => {
  try {
    const query = `select * from users`;
    const result = await db.manyOrNone(query, []);
    return result;
  } catch (error) {
    console.log('Error occurred in getAllUserData', error);
    throw error;
  }
};

export default {
  add,
  getById,
  getByEmailId,
  userLogin,
  getByUserName,
  getAllUserData,
};
