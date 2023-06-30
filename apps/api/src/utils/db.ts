import pgPromise from 'pg-promise';

const initOptions = {
  schema: ['public'],
};

const pgp = pgPromise(initOptions);
const db = pgp(process.env.DEV_DATABASE_URL);
// const db = pgp('postgres://zpaisa:Zpaisa@789@192.168.2.12:5432/zpaisa_db');

export default db;
