import db from './../utils/connection';



const getAllUserData =async () => {
    try {
     const queryText = `select * from users`;
     const result= await db.query(queryText, []);
        return result.rows;
    }
    catch (error) {
        console.log("Error occurred in getMasterDataByType", error)
        throw error;
    }

}





export {
    getAllUserData
}