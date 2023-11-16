import { useQuery, useMutation, useQueryClient } from 'react-query';
import SiteService from '../service/site-service';

const useGetAllSiteDrop = () => {
  return useQuery(['useGetAllSite'], () => SiteService.getAllSiteDrop(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useGetAllSiteDrops = () => {
  return useQuery(['useGetAllSiteDrop'], () => SiteService.getAllSiteDrop(), {
    select: (data) =>
      data?.data?.map((site_id: any) => ({
        value: site_id.site_contractor_id,
        label: site_id.name,
      })),
  });
};

const useCreateSite = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SiteService.createNewSite(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries([]);
      },
    }
  );
};

const useInstantCreateSite = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SiteService.createNewSite(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllSite']);
      },
    }
  );
};

const useGetBySearchSiteData = () => {
  return useMutation((data: any) => {
    return SiteService.filterSiteData(data);
  });
};

const useGetAllPaginatedContractorsData = (data: any) => {
  return useQuery(
    ['useGetAllPaginatedContractorsData'],
    () => SiteService.filterContractorData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useGetBySiteId = (id: number) => {
  return useQuery(['getByuserID', id], () => SiteService.getOneSiteById(id), {
    select: (data) => data.data,
  });
};

const useUpdateSite = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SiteService.updateSiteData(data);
    },
    {
      onSuccess: (response) => {
        queryClient.invalidateQueries(['useGetAllPaginatedContractorsData']);
      },
    }
  );
};

const useDeleteSite = () => {
  return useMutation(
    (data: any) => {
      return SiteService.deleteSite(data);
    },
    {
      onSuccess: () => {
        useGetBySearchSiteData().mutate({});
      },
    }
  );
};

export {
  useGetAllSiteDrop,
  useGetAllSiteDrops,
  useCreateSite,
  useGetBySearchSiteData,
  useGetBySiteId,
  useUpdateSite,
  useDeleteSite,
  useInstantCreateSite,
  useGetAllPaginatedContractorsData,
};
