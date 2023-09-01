import express from 'express';
import {
    bomCreateValidator,
    bomUpdateValidator,
} from '../../validations/bom';
import {
    createBom,
    deleteBom,
    getByBomId,
    updateBom,
    getAllBom,
    fetchEntireDataByBomId,
} from '../../controller/bom.controller';
import authMiddleware from '../../middleware/auth';
import { runValidation } from '../../validations/index';


const router = express.Router();

router.post(
    '/',
    authMiddleware,
    bomCreateValidator,
    runValidation,
    createBom
);
router.put(
    '/',
    authMiddleware,
    bomUpdateValidator,
    runValidation,
    updateBom
);

router.get('/get-all', authMiddleware, getAllBom);

router.get('/get/:bom_id', authMiddleware, getByBomId);

router.delete('/delete/:bom_id', authMiddleware, deleteBom);

router.get('/get-entire-data/:bom_id', fetchEntireDataByBomId);

export default router;