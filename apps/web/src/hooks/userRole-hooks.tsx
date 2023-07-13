import { useQuery, useMutation, useQueryClient } from 'react-query';
import userRoleService from '../service/userRole-service';

const useGetAllRoles = () => {
  return useQuery(['useGetAllRoles'], () => userRoleService.getAllRoles(), {
    select: (data) => data.data,
  });
};

export { useGetAllRoles };
