import express from 'express';
import authMiddleware from '../../middleware/auth';
import {createProduct,getAllProduct,getByProductId,deleteByProductId,updateProduct, addBulkProduct} from '../../controller/product.controller';
import { productUpdateValidator } from '../../validations/product';
import { runValidation } from '../../validations/index';
const router = express.Router();


router.post('/createProduct', authMiddleware, createProduct);
router.post('/addBulkProduct', authMiddleware, addBulkProduct);
router.get('/getAllProduct',authMiddleware, getAllProduct);
router.get('/get/:product_id',authMiddleware, getByProductId);
router.delete('/delete/:product_id',authMiddleware,deleteByProductId);
router.put('/updateProduct', authMiddleware, productUpdateValidator, runValidation,updateProduct );
export default router;