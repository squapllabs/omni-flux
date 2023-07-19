import roleDao from '../dao/role.dao';
import { createRoleBody, updateRoleBody } from '../interfaces/role.Interface';

/**
 * Method to Create a New Role
 * @param body
 * @returns
 */
const createRole = async (body: createRoleBody) => {
  try {
    const { role_name, created_by } = body;
    const roleDetails = await roleDao.add(role_name, created_by);
    const result = { success: true, data: roleDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in role service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Role
 * @param body
 * @returns
 */

const updateRole = async (body: updateRoleBody) => {
  try {
    const { role_name, updated_by, role_id } = body;
    let result = null;
    const roleExist = await roleDao.getById(role_id);
    if (roleExist) {
      const roleDetails = await roleDao.edit(role_name, updated_by, role_id);
      result = { success: true, data: roleDetails };
      return result;
    } else {
      result = { success: false, message: 'role_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in role service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Role By RoleId
 * @param roleId
 * @returns
 */
const getById = async (roleId: number) => {
  try {
    let result = null;
    const roleData = await roleDao.getById(roleId);
    if (roleData) {
      result = { success: true, data: roleData };
      return result;
    } else {
      result = { success: false, message: 'role_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById role service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Roles
 * @returns
 */
const getAllRoles = async () => {
  try {
    const result = await roleDao.getAll();
    const roleData = { success: true, data: result };
    return roleData;
  } catch (error) {
    console.log('Error occurred in getAllRoles role service : ', error);
    throw error;
  }
};

/**
 * Method to delete role
 * @param roleId
 */
const deleteRole = async (roleId: number) => {
  try {
    const roleExist = await roleDao.getById(roleId);

    if (!roleExist) {
      const result = { success: false, message: 'role_id does not exist' };
      return result;
    }
    const data = await roleDao.deleteRole(roleId);
    if (data) {
      const result = {
        success: true,
        message: 'Role Data Deleted Successfully',
      };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this role' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteRole role service : ', error);
    throw error;
  }
};

export { createRole, updateRole, getAllRoles, getById, deleteRole };
