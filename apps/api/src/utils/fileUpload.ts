import fs from 'fs';
import s3Access from './s3';

const processFileUpload = async (req, res) => {
  const storage = req.body.storage;
  const files = req.files;
  const user_id = req.body.user_id;
  let data = null;

  if (storage === 's3') {
    for (const file of files.file) {
      data = await s3Access.uploadFileInS3(file, user_id);
      const uploadedFile = file;
      const filePath = uploadedFile.path;
      fs.unlink(filePath, (err) => {
        if (err) {
          console.error('Error deleting the local file:', err);
        } else {
          console.log('Local File deleted successfully.');
        }
      });
    }
  }
  res.status(200).json({
    message:
      storage === 'local'
        ? 'File upload successful in Local!'
        : 'File upload successful in S3!',
    status: true,
    data: storage === 'local' ? files : data?.path,
  });
};

export default processFileUpload;
