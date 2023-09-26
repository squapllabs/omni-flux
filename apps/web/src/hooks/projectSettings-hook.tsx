import { useQuery, useMutation, useQueryClient } from 'react-query';
import ProjectSettingsService from '../service/projectSettings-service';

const useGetRoleBasedUser = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectSettingsService.fetchRoleBasedUser(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};


const createProjectMember = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectSettingsService.addProjectMember(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllProjectPaginatedData']);
      },
    }
  );
};


const getBySearchProjectMembers = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectSettingsService.filterProjectMember(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useDeleteProjectMember = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectSettingsService.deleteProjectMember(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllProjectPaginatedData']);
      },
    }
  );
};


const useGetAllPaginatedProjectMember = (data: any) => {
  return useQuery(
    ['useGetAllProjectPaginatedData'],
    () => ProjectSettingsService.filterProjectMember(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );

};

export { useGetRoleBasedUser, createProjectMember, getBySearchProjectMembers, useGetAllPaginatedProjectMember, useDeleteProjectMember };