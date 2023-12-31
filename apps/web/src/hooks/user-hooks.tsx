import { useQuery, useMutation, useQueryClient } from 'react-query';
import userService from '../service/user-service';

const useGetAllUsers = () => {
  return useQuery(['useGetAllUsers'], () => userService.getAllUsers(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useGetAllUsersDrop = () => {
  return useQuery(['useGetAllUsersDrop'], () => userService.getAllUsers(), {
    select: (data) =>
      data?.data?.data?.map((user: any) => ({
        value: user.user_id,
        label: user.first_name + ' ' + user.last_name,
      })),
  });
};

const useGetAllPaginatedUser = (data: any) => {
  return useQuery(['useGetAllUsers'], () => userService.filterUser(data), {
    select: (data) => data,
    staleTime: Infinity,
  });
};

const useGetAllInactiveUsers = () => {
  return useQuery(
    ['useGetAllInactiveUsers'],
    () => userService.getAllInactiveUsers(),
    {
      select: (data) => data.data,
    }
  );
};

const useGetByloginID = (id: string) => {
  return useQuery(['getByLoginID', id], () => userService.getOneUser(id));
};
const useGetUserbyRole = (type: string) => {
  return useQuery(
    ['getUserbyRole', type],
    () => userService.getuserByRoleType(type),
    {
      select: (data) =>
        data?.data?.map((user: any) => ({
          value: user.user_id,
          label: user.first_name + ' ' + user.last_name,
        })),
      // data.data,
    }
  );
};
const useGetByuserID = (id: number) => {
  return useQuery(['getByuserID', id], () => userService.getOneUserbyID(id), {
    select: (data) => data.data,
  });
};
const useCreateUser = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return userService.createuser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};

const useUpdateUser = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return userService.updateUser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};

const useDeleteUsers = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return userService.deleteUser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};
const useGetByUser = () => {
  return useMutation((data: any) => {
    return userService.filterUser(data);
  });
};
export {
  useGetAllUsers,
  useGetByloginID,
  useGetByuserID,
  useCreateUser,
  useUpdateUser,
  useDeleteUsers,
  useGetAllInactiveUsers,
  useGetAllUsersDrop,
  useGetByUser,
  useGetAllPaginatedUser,
  useGetUserbyRole,
};
