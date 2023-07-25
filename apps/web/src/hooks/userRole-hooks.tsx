import { useQuery, useMutation, useQueryClient } from 'react-query';
import userRoleService from '../service/userRole-service';

const useGetAllRoles = () => {
  return useQuery(['useGetAllRoles'], () => userRoleService.getAllRoles(), {
    select: (data) =>
      data?.data?.map((role: any) => ({
        value: role.role_id,
        label: role.role_name,
      })),
    refetchOnMount: true,
    refetchOnWindowFocus: true,
    staleTime: 60000,
  });
};

export { useGetAllRoles };
