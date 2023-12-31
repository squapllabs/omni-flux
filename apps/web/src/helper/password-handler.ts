import { AES, enc } from 'crypto-js';
import { environment } from '../environment/environment';

const secretKey = environment.auth_secretKey;

// Function to encrypt a password using AES
export const encryptPassword = (password: string): string => {
  const encryptedPassword = AES.encrypt(password, secretKey).toString();
  return encryptedPassword;
};

// Function to decrypt an encrypted password using AES
export const decryptPassword = (encryptedPassword: string): string => {
  const decryptedPassword = AES.decrypt(encryptedPassword, secretKey).toString(
    enc.Utf8
  );
  return decryptedPassword;
};
