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

const createProject = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.createProjectData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries([]);
      },
    }
  );
};

const getByProject = () => {
  const queryClient = useQueryClient();
  return useMutation (
    (data:any) => {
      return ProjectService.filterProject(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  )
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

const getByProjectId = (id: number) => {
  return useQuery(['getByuserID', id], () => ProjectService.getOneProjectById(id), {
    select: (data) => data.data,
  });
};

const updateProject = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ProjectService.updateProjectData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
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
  return useQuery(['useGetAllProjectManager'], () => ProjectService.getAllProjectManagers(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};
export { useGetAllProject,createProject,getByProject,useDeleteProjects,getByProjectId,updateProject,useGetMasterProjectParentType,useGetAllProjectManagers };
