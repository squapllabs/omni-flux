import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';


const fetchRoleBasedUser = async (data: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/user/get-users-by-role-name/${data?.role}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in fetchRoleBasedUser :', error);
    throw error;
  }
};

const addProjectMember = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project-member-association/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in post ProjectMember :', error);
    throw error;
  }
};

const filterProjectMember = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project-member-association/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in category filterProjectMember :', error);
    throw error;
  }
};

const deleteProjectMember = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/project-member-association/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete Project Member :', error);
    throw error;
  }
};

const fetchProjectUser = async (user_id:number,project_id:number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-member-association/get-by-project-id-and-user-id/${project_id}/${user_id}`
    );    
    return response.data;
  } catch (error) {
    console.log('Error in occur in fetchProjectUser :', error);
    throw error;
  }
};

const fetchAllProjectMembers = async (project_id:number) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-member-association/get-by-project-id/${project_id}`
    );  
    return response.data;
  } catch (error) {
    console.log('Error in occur in fetchAllProjectMember :', error);
    throw error;
  }
};


export default { fetchRoleBasedUser, addProjectMember, filterProjectMember, deleteProjectMember,fetchProjectUser,fetchAllProjectMembers };