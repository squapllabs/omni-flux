import s3Access from './s3';
import fs from 'fs';

const processFileUpload = async (req, res) => {
  const storage = req.body.storage;
  const files = req.files;

  let data = null;
  const allFilePath = [];
  let index = 0;
  if (storage === 's3') {
    for (const file of files.file) {
      data = await s3Access.uploadFileInS3(file);
      const uploadedFile = file;
      const filePath = uploadedFile.path;
      const s3FilePath = data.path;

      allFilePath.push({ index, path: s3FilePath });

      fs.unlink(filePath, (err) => {
        if (err) {
          console.error('Error deleting the local file:', err);
        } else {
          console.log('Local File deleted successfully.');
        }
      });

      index++;
    }
  }
  res.status(200).json({
    message:
      storage === 'local'
        ? 'File upload successful in Local!'
        : 'File upload successful in S3!',
    status: true,
    data: storage === 'local' ? files : allFilePath,
  });
};

export default processFileUpload;
