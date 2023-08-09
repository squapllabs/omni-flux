import express from 'express';
import multer from 'multer';
import authMiddleware from '../../middleware/auth';
import processFileUpload from '../../utils/fileUpload';

const router = express.Router();

const storage = multer.diskStorage({
  destination: function (req, file, cb) {
    const storage = req.body.storage;
    const uploadPath =
      storage === 'local' ? process.env.FILE_UPLOAD_LOCAL_PATH : 'tmp/';
    cb(null, uploadPath);
  },
  filename: function (req, file, cb) {
    const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1e9);
    cb(null, file.fieldname + '-' + uniqueSuffix + '-' + file.originalname);
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

export default router;
