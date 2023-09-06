import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
    machineryCreateValidator,
    machineryUpdateValidator,
} from '../../validations/machinery';
import { runValidation } from '../../validations/index';
import {
    createMachinery,
    updateMachinery,
    getAllMachinery,
    getByMachineryId,
    deleteByMachineryId,
    searchMachinery,
} from '../../controller/machinery.controller';

const router = express.Router();

router.post(
    '/',
    authMiddleware,
    machineryCreateValidator,
    runValidation,
    createMachinery
);

router.put(
    '/',
    authMiddleware,
    machineryUpdateValidator,
    runValidation,
    updateMachinery
);

router.get('/get-all', authMiddleware, getAllMachinery);

router.get('/get/:machinery_id', authMiddleware, getByMachineryId);

router.delete('/delete/:machinery_id', authMiddleware, deleteByMachineryId);

router.post('/search', authMiddleware, searchMachinery);

export default router;
