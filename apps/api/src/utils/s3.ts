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
const uploadFileInS3 = async (
  file: { path: string; filename: string },
  code: string,
  folder: string
) => {
  const fileStream = createReadStream(file.path);
  const uniqueFolder = folder ? folder : 'common';
  const uniqueCode = code ? code : 'default';
  const filePath = `${uniqueFolder}/${uniqueCode}/${file.filename}`;

  const uploadParams = {
    Bucket: bucketName,
    Body: fileStream,
    Key: filePath,
  };

  try {
    const response = await s3.send(new PutObjectCommand(uploadParams));
    const objectURL = `https://${bucketName}.s3.${region}.amazonaws.com/${filePath}`;
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

/**
 * Method for deleting a file from S3 using path
 * @param filePath - The URL/path of the file to be deleted
 * @returns - A promise that resolves to the S3 response
 */
const deleteFileFromS3UsingPath = async (filePath) => {
  const urlParts = filePath.split('/');
  const objectKey = urlParts[urlParts.length - 1];

  const deleteParams = {
    Bucket: bucketName,
    Key: objectKey,
  };

  try {
    const command = new DeleteObjectCommand(deleteParams);
    const response = await s3.send(command);
    return response;
  } catch (err) {
    console.log(err, err.stack);
    throw err;
  }
};

export default { uploadFileInS3, deleteS3File, deleteFileFromS3UsingPath };
