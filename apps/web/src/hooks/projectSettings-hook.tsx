import { useQuery, useMutation, useQueryClient } from 'react-query';
import ProjectSettingsService from '../service/projectSettings-service';
import projectSettingsService from '../service/projectSettings-service';

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
        queryClient.invalidateQueries(['useGetAllInitialProjectMember']);
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

const useGetAllPaginatedProjectMember = (data: any) => {
  return useQuery(
    ['useGetAllInitialProjectMember'],
    () => ProjectSettingsService.filterProjectMember(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useDeleteProjectMember = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSettingsService.deleteProjectMember(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllInitialProjectMember']);
      },
    }
  );
};


export { useGetRoleBasedUser, createProjectMember, getBySearchProjectMembers, useGetAllPaginatedProjectMember, useDeleteProjectMember };