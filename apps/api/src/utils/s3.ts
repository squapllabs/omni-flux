import {
  S3Client,
  PutObjectCommand,
  DeleteObjectCommand,
} from '@aws-sdk/client-s3';
import { createReadStream } from 'fs';

const bucketName = process.env.AWS_BUCKET_NAME;
const region = process.env.AWS_BUCKET_REGION;
const secretAccessKey = process.env.AWS_SECRET_ACCESS_KEY;
const accessKeyId = process.env.AWS_ACCESS_KEY_ID;

const s3 = new S3Client({
  region,
  credentials: {
    accessKeyId,
    secretAccessKey,
  },
});

/**
 * Method for uploading a new file to S3
 * @param file - The file to be uploaded
 * @returns - A promise that resolves to the S3 response
 */
const uploadFileInS3 = async (file: { path: string; filename: string }) => {
  const fileStream = createReadStream(file.path);
  const uploadParams = {
    Bucket: bucketName,
    Body: fileStream,
    Key: file.filename,
  };

  try {
    const response = await s3.send(new PutObjectCommand(uploadParams));
    const objectURL = `https://${bucketName}.s3.${region}.amazonaws.com/${file.filename}`;
    const result = { response: response, path: objectURL };
    return result;
  } catch (err) {
    console.log(err, err.stack);
    throw err;
  }
};

/**
 * Method for deleting an existing file from S3
 * @param doc - The document containing the key of the file to be deleted
 * @returns {Promise<any>} - A promise that resolves to the S3 response
 */
const deleteS3File = async (doc: { key: string }) => {
  const { key } = doc;
  const params = {
    Bucket: bucketName,
    Key: key,
  };
  try {
    return await s3.send(new DeleteObjectCommand(params));
  } catch (err) {
    console.log(err, err.stack);
    throw err; // Rethrow the error so the caller can handle it
  }
};

export default { uploadFileInS3, deleteS3File };