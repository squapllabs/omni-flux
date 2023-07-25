import express from 'express';
import authMiddleware from '../../middleware/auth';
import {addItemType,getAllItemType,getByItemTypeId,deleteByItemTypeId,updateItemType} from '../../controller/itemType.controller';
import { itemTypeUpdateValidator } from '../../validations/itemType';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/add-item-type', authMiddleware, addItemType);
router.get('/get-all-item-types',authMiddleware,getAllItemType );
router.get('/get/:item_type_id',authMiddleware, getByItemTypeId);
router.delete('/delete/:item_type_id',authMiddleware,deleteByItemTypeId);
router.put('/update-item-type', authMiddleware, itemTypeUpdateValidator, runValidation,updateItemType );

export default router;