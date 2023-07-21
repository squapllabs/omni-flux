import express from 'express';
import authMiddleware from '../../middleware/auth';
import {addItem,updateItem,deleteByItemId,getAllItem,getByItemId,addBulkItem,getAllItemBySearch} from '../../controller/item.controller';
import { itemUpdateValidator } from '../../validations/item';
import { runValidation } from '../../validations/index';
const router = express.Router();


router.post('/add-item', authMiddleware, addItem);
router.post('/addBulkItem', authMiddleware, addBulkItem);
router.post('/get-all-items',authMiddleware, getAllItem);
router.get('/get/:item_id',authMiddleware, getByItemId);
router.delete('/delete/:item_id',authMiddleware,deleteByItemId);
router.put('/update-item', authMiddleware, itemUpdateValidator, runValidation,updateItem );
router.post('/get-all-items-by-search', authMiddleware,getAllItemBySearch);

export default router;