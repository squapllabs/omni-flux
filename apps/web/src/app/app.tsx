// eslint-disable-next-line @typescript-eslint/no-unused-vars
import styles from './app.module.scss';

import NxWelcome from './nx-welcome';

import { useGetAllUsers } from '../hooks/user-hooks';
import Route from '../layout/route';
export function App() {
  const { data: getAllUsers } = useGetAllUsers();
  console.log("data", getAllUsers);

  return (
    <div>
      <Route />
    </div>
  );
}

export default App;
