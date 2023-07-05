import authDao from '../dao/auth.dao';
import userDao from "../dao/user.dao";
const jwt = require("jsonwebtoken");
const md5 = require('md5');
import { AES, enc } from 'crypto-js';

/**
 * Method for forget password url generate
 * @param body
 * @returns
 */
const forgetPassword = async (body: { email_id: string }) => {
    try {
        let result = null;
        const { email_id } = body;
        console.log("body==>", body);
        const userCheckExist = await userDao.getByEmailId(email_id);
        if (userCheckExist) {
            let userId = userCheckExist.user_id;
            const token = jwt.sign({ userId, email_id }, process.env.NEXT_PUBLIC_FORGETPASSWORD_TOKEN_SECRET_KEY, { expiresIn: '2h' });
            const link = `${process.env.REACT_APP_URL}/reset-password/${userId}/${token}/`
            console.log("link ==>", link);
            return (result = { success: true, data: userCheckExist });
        } else {
            return (result = { success: false, message: 'user email not exist' });
        }
    } catch (error) {
        console.log('Error occurred in forget password : ', error);
        throw error;
    }
};

const updatePassword = async (body: { email_id: string, user_password: string }) => {
    try {
        let result = null;
        const { email_id, user_password } = body;
        const userCheckExist = await userDao.getByEmailId(email_id);
        if (userCheckExist) {
            const decryptedPassword = AES.decrypt(user_password, process.env.AUTH_SECRET_KEY).toString(enc.Utf8);
            result = await authDao.editPassword(email_id, md5(decryptedPassword));
            // console.log("result",result);
            return (result = { success: true, data: result });  
        } else {
            return (result = { success: false, message: 'user email not exist' });
        }
    } catch (error) {
        console.log('Error occurred in update password : ', error);
        throw error;
    }
};

export { forgetPassword, updatePassword };
