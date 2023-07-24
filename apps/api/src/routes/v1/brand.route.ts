import express from 'express';
import authMiddleware from '../../middleware/auth';
import {addBrand,getAllBrand,getByBrandId,deleteByBrandId,updateBrand} from '../../controller/brand.controller';
import { brandUpdateValidator } from '../../validations/brand';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post('/add-brand', authMiddleware, addBrand);
router.get('/get-all-brands',authMiddleware,getAllBrand );
router.get('/get/:brand_id',authMiddleware, getByBrandId);
router.delete('/delete/:brand_id',authMiddleware,deleteByBrandId);
router.put('/update-brand', authMiddleware, brandUpdateValidator, runValidation,updateBrand );

export default router;