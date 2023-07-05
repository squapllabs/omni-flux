// import forgetpasswordDao from '../dao/auth.dao';
import userDao from "../dao/user.dao";
const jwt = require("jsonwebtoken")

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
            const token = jwt.sign({userId, email_id}, process.env.NEXT_PUBLIC_FORGETPASSWORD_TOKEN_SECRET_KEY, { expiresIn: '2h' });
            const link = `http://localhost:8080/resetpassword/${token}/`
            console.log("link ==>",link);
            return (result = { success: true, data: userCheckExist });
        } else {
            return (result = { success: false, message: 'user email not exist' });
        }
    } catch (error) {
        console.log('Error occurred in forget password : ', error);
        throw error;
    }
};


export { forgetPassword };
