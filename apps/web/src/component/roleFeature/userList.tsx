import React from 'react';
import {
  useGetAllUsers,
  // useDeleteUsers,
  // useGetByUser,
  // useGetAllPaginatedUser,
} from '../../hooks/user-hooks';

const UserList = () => {
  const { isLoading: getAllLoading, data } = useGetAllUsers();
  console.log(data);
  return (
    <div>
      <h2>User Table</h2>
      <table>
        <thead>
          <tr>
            <th>User ID</th>
            <th>First Name</th>
            <th>Last Name</th>
            <th>Contact Number</th>
          </tr>
        </thead>
        <tbody>
          {data && data.length > 0 ? (
            data.map((user: any) => (
              <tr key={user.user_id}>
                <td>{user.user_id}</td>
                <td>{user.first_name}</td>
                <td>{user.last_name}</td>
                <td>{user.contact_no}</td>
              </tr>
            ))
          ) : (
            <tr>
              <td>No users found</td>
            </tr>
          )}
        </tbody>
      </table>
    </div>
  );
};

export default UserList;
