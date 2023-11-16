import { useQuery, useMutation, useQueryClient } from 'react-query';
import projectSiteService from '../service/projectSite-service';

const useGetAllprojectSite = () => {
  return useQuery(
    ['useGetAllprojectSite'],
    () => projectSiteService.getAllprojectSite(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetAllprojectSiteDrop = () => {
  return useQuery(
    ['useGetAllprojectSiteDrop'],
    () => projectSiteService.getAllprojectSite(),
    {
      select: (data) =>
        data?.data?.map((projectSite: any) => ({
          value: projectSite.projectSite_id,
          label: projectSite.name,
        })),
    }
  );
};

const useGetOneprojectSiteyID = (id: number) => {
  return useQuery(
    ['getOneprojectSiteyID', id],
    () => projectSiteService.getOneprojectSiteByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const useCreateprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.createprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteData']);
      },
    }
  );
};

const useInstantcreateprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.createprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteDrop']);
      },
    }
  );
};

const useUpdateprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.updateprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteData']);
      },
    }
  );
};

const useDeleteprojectSite = () => {
  const queryprojectSite = useQueryClient();
  return useMutation(
    (data: any) => {
      return projectSiteService.deleteprojectSite(data);
    },
    {
      onSuccess: () => {
        queryprojectSite.invalidateQueries(['useGetAllprojectSiteData']);
      },
    }
  );
};
const useGetByprojectSite = () => {
  return useMutation((data: any) => {
    return projectSiteService.filterprojectSite(data);
  });
};

const useGetAllPaginatedprojectSite = (data: any) => {
  return useQuery(
    ['useGetAllprojectSiteData'],
    () => projectSiteService.filterprojectSite(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

export {
  useGetAllprojectSite,
  useGetOneprojectSiteyID,
  useCreateprojectSite,
  useUpdateprojectSite,
  useDeleteprojectSite,
  useGetAllprojectSiteDrop,
  useGetByprojectSite,
  useInstantcreateprojectSite,
  useGetAllPaginatedprojectSite,
};
