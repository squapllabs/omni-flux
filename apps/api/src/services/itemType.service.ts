import itemTypeDao from '../dao/itemType.dao';
import { createItemTypeBody, updateItemTypeBody } from '../interfaces/itemType.interface';
import prisma from '../utils/prisma';

/**
 * Method to Add a new itemType
 * @param body
 * @returns
 */
const addItemType = async (body: createItemTypeBody) => {
    let result = null;
    try {
      const item_type_item_code = body.item_type_item_code;
      const item_type_item_name = body.item_type_item_name;
      const created_by = body.created_by;
      const updated_by = body.updated_by;
      result = await prisma
        .$transaction(async (prisma) => {
          const CreateItemType = await itemTypeDao.add(
            item_type_item_code,
            item_type_item_name,
            created_by,
            updated_by,
            prisma
          );
  
          return CreateItemType;
        })
        .then((data) => {
          console.log('Successfully itemType Data Returned ', data);
          const newItemTypeData = {
            success: true,
            data: data,
          };
          return newItemTypeData;
        })
        .catch((error: string) => {
          console.log('Failure, ROLLBACK was executed', error);
          throw error;
        });
      return result;
    } catch (error) {
      console.log('Error occurred in ItemType service: ', error);
      throw error;
    }
  };
/**
 * Method to Get All ItemType
 * @returns
 */
const getAllItemType = async () => {
    try {
      const result = await itemTypeDao.getAll(
      );
      const itemTypeData = { success: true, data: result };
      return itemTypeData;
    } catch (error) {
      console.log('Error occurred in getAll itemType service: ', error);
      throw error;
    }
  };

/**
 * Method to get itemType By itemTypeId
 * @param itemTypeIdId
 * @returns
 */
const getById = async (item_type_id: number) => {
    try {
      let result = null;
      const itemTypeData = await itemTypeDao.getById(item_type_id);
      if (itemTypeData) {
        result = { success: true, data: itemTypeData };
        return result;
      } else {
        result = { success: false, message: 'itemType Id not exist' };
        return result;
      }
    } catch (error) {
      console.log('Error occurred in getById itemType service : ', error);
      throw error;
    }
  };  
 /**
 * Method to delete itemType
 * @param itemTypeId
 */
const deleteItemType = async (item_type_id: number) => {
    try {
      const itemTypeExist = await itemTypeDao.getById(item_type_id);
      if (!itemTypeExist) {
        const result = { success: false, message: 'itemType Id Not Exist' };
        return result;
      }
      const data = await itemTypeDao.deleteItemType(item_type_id);
      if (data) {
        const result = {
          success: true,
          message: 'itemType Data Deleted Successfully',
        };
        return result;
      } else {
        const result = { success: false, message: 'Failed to delete this itemType' };
        return result;
      }
    } catch (error) {
      console.log('Error occurred in deleteItemType itemType service : ', error);
      throw error;
    }
  }; 
  /**
 * Method to Update an Existing itemType
 * @param body
 * @returns
 */
const updateItemType = async (body: updateItemTypeBody) => {
    try {
      const item_type_id = body.item_type_id;
      const item_type__item_code = body.item_type_item_code;
      const item_type__item_name = body.item_type_item_name;
      const updated_by = body.updated_by;
      let result = null;
      const itemTypeExist = await itemTypeDao.getById(item_type_id);
      if (itemTypeExist) {
        const itemTypeDetails = await itemTypeDao.edit(
          item_type_id,
          item_type__item_code,
          item_type__item_name,
          updated_by,
        );
        result = { success: true, data: itemTypeDetails };
        return result;
      } else {
        result = { success: false, message: 'itemType_id not exist' };
        return result;
      }
    } catch (error) {
      console.log('Error occurred in itemType service Edit: ', error);
      throw error;
    }
  };
  export {
    addItemType,
    getAllItemType,
    getById,
    deleteItemType,
    updateItemType
  }