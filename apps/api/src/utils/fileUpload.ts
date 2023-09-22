import s3Access from './s3';
import fs from 'fs';

const processFileUpload = async (req) => {
  console.log("service call:",)
  const storage = req.body.storage;
  const code = req.body.code;
  const folder = req.body.folder;
  const files = req.files;

  let data = null;
  const allFilePath = [];
  let index = 0;
  if (storage === 's3') {
    for (const file of files.file) {
      data = await s3Access.uploadFileInS3(file, code, folder);
      console.log("Check uploaded data:", data)
      const uploadedFile = file;
      const filePath = uploadedFile.path;
      const s3FilePath = data.path;

      allFilePath.push({ index, path: s3FilePath });
      console.log("test 1:", filePath)
      // fs.unlink(filePath, (err) => {
      //   if (err) {
      //     console.error('Error deleting the local file:', err);
      //   } else {
      //     console.log('Local File deleted successfully.');
      //   }
      // });

      index++;
    }
  }

  const result = {
    message:
      storage === 'local'
        ? 'File upload successful in Local!'
        : 'File upload successful in S3!',
    status: true,
    data: storage === 'local' ? files : allFilePath,
  };
  return result;
};

const processFileDeleteInS3 = async (body) => {
  const { path } = body;
  await s3Access.deleteFileFromS3UsingPath(path);
  const result = {
    message: 'File Deleted Successfully in S3!',
    status: true,
    data: null,
  };
  return result;
};

export { processFileUpload, processFileDeleteInS3 };
