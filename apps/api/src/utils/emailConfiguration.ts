import nodemailer from 'nodemailer';

const transporter = nodemailer.createTransport({
  port: process.env.EMAIL_PORT,
  host: process.env.EMAIL_HOST,
  maxConnections: process.env.EMAIL_MAX_CONNECTIONS,
  auth: {
    user: process.env.EMAIL_USER,
    pass: process.env.EMAIL_PASSWORD,
  },
});

export default { transporter };
