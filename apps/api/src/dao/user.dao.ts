import db from './../utils/connection';

const add = async (
  center_id: BigInteger,
  username: string,
  userpass: string,
  mobilenumber: string,
  email: string,
  firstname: string,
  lastname: string,
  profileimgurl: string,
  gender: string,
  dob: Date,
  status: string,
  address: string,
  createdby: BigInteger,
  updatedby: BigInteger,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const currentDate = new Date();
    const query = `INSERT INTO users(center_id,username,userpass,mobilenumber,
        email,firstname,lastname,profileimgurl,gender,dob,status,address,createdby,
        createddatetime,updatedby,updateddatetime) 
          values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16) RETURNING *`;
    const result = await transaction.query(query, [
      center_id,
      username,
      userpass,
      mobilenumber,
      email,
      firstname,
      lastname,
      profileimgurl,
      gender,
      dob,
      status,
      address,
      createdby,
      currentDate,
      updatedby,
      currentDate,
    ]);
    return result.rows;
  } catch (error) {
    console.log('Error occurred in userDao add', error);
    throw error;
  }
};

const getById = async (userId: bigint) => {
  try {
    let result = null;
    const query = `select * from users where id=$1`;
    result = await db.query(query, [userId]);
    return result.rows;
  } catch (error) {
    console.log('Error occurred in getById', error);
    throw error;
  }
};

const getByEmailId = async (emailId: string) => {
  try {
    let result = null;
    const query = `select * from users where email=$1`;
    result = await db.query(query, [emailId]);
    return result.rows;
  } catch (error) {
    console.log('Error occurred in getById', error);
    throw error;
  }
};

const userLogin = async (email: string, userpass: string) => {
  try {
    let result = null;
    const query = `select * from users where email=$1 and userpass=$2`;
    result = await db.query(query, [email, userpass]);
    return result.rows;
  } catch (error) {
    console.log('Error occurred in getById', error);
    throw error;
  }
};

const getAllUserData = async () => {
  try {
    const query = `select * from users`;
    const result = await db.query(query, []);
    return result.rows;
  } catch (error) {
    console.log('Error occurred in getAllUserData', error);
    throw error;
  }
};

export { add, getById, getByEmailId, userLogin, getAllUserData };
