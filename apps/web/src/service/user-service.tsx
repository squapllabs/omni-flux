import axios from 'axios';

const getAllUsers = async () => {
    try {
        const response = await axios.get(`http://localhost:8080/api/user/getAll`, {
            headers: {
                "Content-Type": "application/json",
                "Access-Control-Allow-Origin": "*",
                'Access-Control-Allow-Methods': 'POST,GET,PUT,OPTIONS',
                'Access-Control-Allow-Headers': '*',
            }
        });
        return response;
    } catch (error) {
        console.log("Error in getting all users:", error);
        throw error;
    }
};

export default {
    getAllUsers
};