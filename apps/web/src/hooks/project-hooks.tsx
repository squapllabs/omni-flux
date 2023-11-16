import { useQuery, useMutation, useQueryClient } from 'react-query';
import ProjectService from '../service/project-service';

const useGetAllProject = () => {
  return useQuery(['useGetAllProject'], () => ProjectService.getAllProject(), {
    select: (data) =>
      data?.data?.map((project: any) => ({
        value: project.project_id,
        label: project.project_name,
      })),
  });
};

const useGetAllProjectStatus = () => {
  return useQuery(['useGetAllProjectStatus'], () =>
    ProjectService.getAllProjectStatus()
  );
};

const useGetDashboardDatasforPO = () => {
  return useQuery(['useGetDashboardDatasforPO'], () =>
    ProjectService.getDashboardDatas()
  );
};

const useGetAllProjectDrop = () => {
  return useQuery(
    ['useGetAllProjectDrop'],
    () => ProjectService.getAllProject(),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project.project_id,
          label: project.project_name,
        })),
    }
  );
};

const useCreateProject = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.createProjectData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};

const useGetByProject = () => {
  return useMutation((data: any) => {
    return ProjectService.filterProject(data);
  });
};
const useGetMemberBasedProject = () => {
  return useMutation((data: any) => {
    return ProjectService.filterProjectmemberBased(data);
  });
};

const useGetPaginatedMemberBasedProject = (data: any) => {
  return useQuery(
    ['getPaginatedMemberBasedProject'],
    () => ProjectService.filterProjectmemberBased(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useDeleteProjects = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.deleteProject(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUsers']);
      },
    }
  );
};

const useGetByProjectId = (id: number) => {
  return useQuery(
    ['getByuserID', id],
    () => ProjectService.getOneProjectById(id),
    {
      select: (data) => data.data,
    }
  );
};
const useGetUserDataProjectRolebased = (value: any) => {
  return useQuery(
    ['getUserDataroleAndProjectBased', value],
    () => ProjectService.getUserDataRolebasedandProjectBased(value),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project?.user_data?.user_id,
          label:
            `${project?.user_data?.first_name}` +
            ' ' +
            `${project?.user_data?.last_name}`,
        })),
    }
  );
};

const useGetUserIDProjectRolebased = (value: any) => {
  return useQuery(
    ['getUserIdroleAndProjectBased', value],
    () => ProjectService.getProjectDataBasedOnUserandRole(value),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project?.project_data?.project_id,
          label: project?.project_data?.project_name,
        })),
    }
  );
};

const useGetUserIDBasedProject = (value: any) => {
  return useQuery(
    ['getUserIDBasedProject', value],
    () => ProjectService.getProjectDataBasedOnUser(value),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project?.project_id,
          label: project?.project_name,
        })),
    }
  );
};

const useUpdateProject = () => {
  return useMutation((data: any) => {
    return ProjectService.updateProjectData(data);
  });
};

const useGetMasterProjectParentType = () => {
  return useQuery(
    ['useGetAllmasertData'],
    () => ProjectService.getAllProjectParentType(),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project.master_data_name,
          label: project.master_data_name,
        })),
    }
  );
};

const useGetAllProjectManagers = () => {
  return useQuery(
    ['useGetAllProjectManager'],
    () => ProjectService.getAllProjectManagers(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetProjectSite = (id: number) => {
  return useQuery(
    ['getProjectSite', id],
    () => ProjectService.getOneProjectSite(id),
    {
      select: (data) =>
        data?.data?.map((options: any) => ({
          value: options.site_id,
          label: options.site_details.name,
        })),
    }
  );
};

export {
  useGetAllProject,
  useGetAllProjectStatus,
  useCreateProject,
  useGetByProject,
  useDeleteProjects,
  useGetByProjectId,
  useUpdateProject,
  useGetMasterProjectParentType,
  useGetAllProjectManagers,
  useGetAllProjectDrop,
  useGetMemberBasedProject,
  useGetPaginatedMemberBasedProject,
  useGetUserDataProjectRolebased,
  useGetProjectSite,
  useGetUserIDProjectRolebased,
  useGetUserIDBasedProject,
  useGetDashboardDatasforPO,
};
