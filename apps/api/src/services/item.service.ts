import itemDao from '../dao/item.dao';
import {createItemBody,updateItemBody} from '../interfaces/item.interface';
import prisma from '../utils/prisma';

/**
 * Method to Create a new item
 * @param body
 * @returns
 */
const createItem = async (body: createItemBody) => {
    let result = null;
    try {
      const {
        item_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
        item_type_id
      } = body;
  
      result = await prisma
        .$transaction(async (prisma) => {
          const CreateItem = await itemDao.add(
            item_name,
            sub_sub_category_id,
            description,
            hsn_code_id,
            gst_id,
            uom_id,
            created_by,
            updated_by,
            item_type_id,
            prisma
          );

          return CreateItem;
        })
        .then((data) => {
          console.log('Successfully item Data Returned ', data);
          const newItemData = {
            success: true,
            data: data,
          };
          return newItemData;
        })
        .catch((error: string) => {
          console.log('Failure, ROLLBACK was executed', error);
          throw error;
        });
      return result;
    } catch (error) {
      console.log('Error occurred in item service Add: ', error);
      throw error;
    }
  };
/**
 * Method to Add a bulk item
 * @param body
 * @returns
 */

const createItemBulk = async (items: createItemBody[]) => {
  try {
    const newItems: any[] = [];
    for (const item of items) {
      const {
        item_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
        item_type_id
      } = item;

      newItems.push({
        item_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
        created_date: new Date(),
        updated_date: new Date(),
        item_type_id
      });
    }

    const result = await itemDao.addBulk(newItems);

    console.log('Successfully inserted bulk item', result);

    return {
      success: true,
      data: result,
    };
  } catch (error) {
    console.log('Error occurred in item service Add: ', error);
    throw error;
  }
};


  /**
 * Method to Get All item
 * @returns
 */

  const getAllItem = async (data) => {
    try {
      const offset = data.offset;
      const limit = data.limit;
      const order_by_column = data.order_by_column;
      const order_by_direction = data.order_by_direction === 'asc' ? 'asc' : 'desc';
      const offsetValue = parseInt(offset, 10) || 0;
      const limitValue = parseInt(limit, 10) || 50;
      const filters = data.filters;
      const filterObj = {
        filterItem: {
          AND: [],
        },
      };
  
      for (const filter of filters) {
        const field_name = filter.field_name;
        const operator = filter.operator;
        const field_value = filter.field_value;
  
       await applyFilter(filterObj, field_name, operator, field_value);
      }
      const result = await itemDao.getAll(offsetValue, limitValue, order_by_column, order_by_direction, filterObj);
      const itemData = { success: true, data: result };
      return itemData;
    } catch (error) {
      console.log('Error occurred in getAll item service: ', error);
      throw error;
    }
  };
  const applyFilter = async (filterObj, field_name, operator, field_value) => {
    if (operator === 'Equal') {
      filterObj.filterItem[field_name] = field_value;
    } else if (operator === 'Not Equal') {
      filterObj.filterItem[field_name] = { NOT: { equals: field_value } };
    } else if (operator === 'Like') {
      filterObj.filterItem[field_name] = { contains: field_value };
    } else if (operator === 'Not Like') {
      filterObj.filterItem[field_name] = { NOT: { contains: field_value } };
    } else if (operator === 'In') {
      filterObj.filterItem[field_name] = { in: field_value };
    } else if (operator === 'Not In') {
      filterObj.filterItem[field_name] = { NOT: { in: field_value } };
    } else if (operator === 'Is') {
      filterObj.filterItem[field_name] = null;
    } else {
      throw new Error(`Unsupported operator: ${operator}`);
    }
  };

 /**
 * Method to Get All Item By search
 * @returns
 */  
const getAllItemBySearch = async()=>{
     try{
      // const SearchItem=data.search;
      let result = null;
      const itemData = await itemDao.getAllBySearch();
      if (itemData) {
        result = { success: true, data: itemData };
        return result;
     }
    }
     catch(error){
      console.log('Error occurred in getAll item service: ', error);
      throw error;
     } 
};







/**
 * Method to get item By itemId
 * @param ItemId
 * @returns
 */
const getById = async (itemId: number) => {
  try {
    let result = null;
    const itemData = await itemDao.getById(itemId);
    if (itemData) {
      result = { success: true, data: itemData };
      return result;
    } else {
      result = { success: false, message: 'item id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById item service : ', error);
    throw error;
  }
};
/**
 * Method to delete item
 * @param itemId
 */
const deleteItem = async (itemId: number) => {
  try {
    const itemExist = await itemDao.getById(itemId);
    if (!itemExist) {
      const result = { success: false, message: 'item Id Not Exist' };
      return result;
    }
    const data = await itemDao.deleteItem(itemId);
    if (data) {
      const result = {
        success: true,
        message: 'item Data Deleted Successfully',
      };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this item' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteItem item service : ', error);
    throw error;
  }
};
/**
 * Method to Update an Existing item
 * @param body
 * @returns
 */
const updateItem = async (body: updateItemBody) => {
  try {
    const { item_id, item_name,sub_sub_category_id , description,  hsn_code_id,gst_id,uom_id,updated_by,item_type_id} = body;
    let result = null;
    const ItemExist = await itemDao.getById(item_id);
    if (ItemExist) {
      const itemDetails = await itemDao.edit(
          item_id,
          item_name,
          sub_sub_category_id,
          description,
          hsn_code_id,
          gst_id,
          uom_id,
          updated_by,
          item_type_id
      );
      result = { success: true, data: itemDetails };
      return result;
    } else {
      result = { success: false, message: 'item_id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in item service Edit: ', error);
    throw error;
  }
};
export {
    createItem,
    getAllItem,
    getById,
    deleteItem,
    updateItem,
    createItemBulk,
    getAllItemBySearch
}