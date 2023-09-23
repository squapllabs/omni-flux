import { useQuery, useMutation, useQueryClient } from 'react-query';
import StockAuditService from '../service/stockaudit-service';

const useGetAllPaginatedStockAudit = (data: any) => {
  return useQuery(
    ['useGetAllStockAuditData'],
    () => StockAuditService.filterStockAudit(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const getByFilterStockAudit = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return StockAuditService.filterStockAudit(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useDeleteStockAudit = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return StockAuditService.deleteStockAudit(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllStockAuditData']);
      },
    }
  );
};

const createStockAudit = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return StockAuditService.createStockAudit(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllStockAuditData']);
      },
    }
  );
};

const updateStockAudit = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return StockAuditService.updateStockAudits(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllStockAuditData']);
      },
    }
  );
};

const getByStockAuditId = (id: number) => {
  return useQuery(
    ['getByuserID', id],
    () => StockAuditService.getOneStockAuditById(id),
    {
      select: (data) => data.data,
    }
  );
};

const useGetAllStockAudits = () => {
  return useQuery(
    ['useGetAllStockAudits'],
    () => StockAuditService.getAllStockAudits(),
    {
      select: (data) =>
        data?.data?.map((StockAudit: any) => ({
          value: StockAudit.StockAudit_id,
          label: StockAudit.StockAudit_name,
        })),
      refetchOnMount: true,
      refetchOnWindowFocus: true,
      staleTime: 60000,
    }
  );
};

const getItemByProjectAndSite = (values : any) => {
  return useQuery(
    ['getItems', values],
    () => StockAuditService.getItems(values),
    {
      select: (data) => data.data,
    }
  );
};

export {
  useGetAllPaginatedStockAudit,
  getByFilterStockAudit,
  useDeleteStockAudit,
  createStockAudit,
  updateStockAudit,
  getByStockAuditId,
  useGetAllStockAudits,
  getItemByProjectAndSite
};
