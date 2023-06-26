import express from 'express';
import db from './utils/connection'

const host = process.env.HOST ?? 'localhost';
const port = process.env.PORT ? Number(process.env.PORT) : 3000;

const app = express();

app.get('/', async (req, res) => {
  const data = await getDetails()
  console.log("check data ---->", data)
  res.send({ message: 'Hello API data', data });
});


const getDetails = async () => {
  const details = await db.query("select * from sale_return")
  return details.rows
}
app.listen(port, host, () => {
  console.log(`[ ready ] http://${host}:${port}`);
});
