import { Pool } from 'pg';
import dotenv from 'dotenv'

dotenv.config();

const pool = new Pool({
  user: process.env.DB_USER,
  host: process.env.DB_HOST,
  database: process.env.DB_DATABASE,
  password: process.env.DB_PASSWORD,
  port: process.env.DB_PORT,
  connectionTimeoutMillis: 10000, // Timeout in milliseconds, e.g., 10000ms = 10 seconds
  idleTimeoutMillis: 30000
});

pool.connect((err, client, release) => {
  if (err) {
    console.error('Error connecting to Database:', err);
    return;
  }
  // Your query logic here
  console.log('Connected...')

});


export default pool;