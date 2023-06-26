import axios from 'axios';
import * as userDao from '../dao/user.dao'

/**
 * Method to get all lookup type
 */
const getAllUser = async () => {
    try {
        const response=userDao.getAllUserData();
        return response;
    } catch (error) {
        console.log("Error occurred in fetchAll user service : ", error);
        throw error;

    }

};

export {
    getAllUser
};
