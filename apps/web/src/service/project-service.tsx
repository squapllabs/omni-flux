import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllProject = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all project data:', error);
    throw error;
  }
};

const getAllProjectStatus = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project/get-dashboard`
    );
    return response.data.data; 
  }
  catch (error) {
    console.log("Error in getAllProjectStatus() SERVICE", error);
    throw error;
  }
}

const createProjectData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project  create  :', error);
    throw error;
  }
};

const checkProjectCodeDuplicate = async (values: any) => {
  const data = values.toUpperCase();
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project/check-duplicate-code/${data}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in check checkSiteCodeDuplicate :', error);
  }
};

const filterProject = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project search :', error);
    throw error;
  }
};
const filterProjectmemberBased = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/project-member-association/search`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in project search :', error);
    throw error;
  }
};
const deleteProject = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/project/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete project list :', error);
    throw error;
  }
};

const getOneProjectById = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project/get/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOne project workbreak down :', error);
    throw error;
  }
};

const updateProjectData = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/project`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in updateProjectData :', error);
  }
};

const getAllProjectParentType = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/master-data/get-by-type/PJTYP`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting getAllProjectParentType:', error);
    throw error;
  }
};

const getAllProjectManagers = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/user/get-users-by-role-name/Project Manager`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};
const getUserDataRolebasedandProjectBased = async (value: any) => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/project-member-association/get-by-project-id-and-role-name/${value?.projectID}/${value?.role}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};
export default {
  getAllProject,
  getAllProjectStatus,
  createProjectData,
  checkProjectCodeDuplicate,
  filterProject,
  deleteProject,
  getOneProjectById,
  updateProjectData,
  getAllProjectParentType,
  getAllProjectManagers,
  filterProjectmemberBased,
  getUserDataRolebasedandProjectBased,
};
