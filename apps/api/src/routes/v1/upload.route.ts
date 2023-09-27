import express from 'express';
import multer from 'multer';
import authMiddleware from '../../middleware/auth';
import {
  processFileDelete,
  processFileUpload,
} from '../../controller/upload.controller';
import { processFileDeleteValidator } from '../../validations/upload';
import { runValidation } from '../../validations';

const router = express.Router();

const storage = multer.diskStorage({
  destination: function (req, file, cb) {
    // const storage = req.body.storage;
    // const uploadPath = 'uploads/';
    // storage === 'local' ? process.env.FILE_UPLOAD_LOCAL_PATH : 'temp-file/';
    cb(null, 'apps/');
  },
  filename: function (req, file, cb) {
    const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1e9);
    cb(null, file.fieldname + '-' + uniqueSuffix + '-' + file.originalname);
    // cb(null, 'Pipe Line Project' + '-' + 'Aalam' + '-' + file.originalname);
  },
});
const upload = multer({ storage });

router.post(
  '/file',
  authMiddleware,
  upload.fields([
    {
      name: 'file',
    },
  ]),
  processFileUpload
);

router.post(
  '/file/delete',
  authMiddleware,
  processFileDeleteValidator,
  runValidation,
  processFileDelete
);

export default router;
