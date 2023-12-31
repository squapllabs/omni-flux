import { useQuery, useMutation, useQueryClient } from 'react-query';
import siteExpenseService from '../service/siteExpanse-service';

const useGetAllsiteExpense = () => {
  return useQuery(
    ['useGetAllsiteExpense'],
    () => siteExpenseService.getAllsiteExpense(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetBysiteExpenseID = (id: number) => {
  return useQuery(
    ['getOnesiteExpenseID', id],
    () => siteExpenseService.getOnesiteExpenseByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const useCreatesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.createsiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useUpdatesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.updatesiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useDeletesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.deletesiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useGetBySearchsiteExpense = () => {
  return useMutation((data: any) => {
    return siteExpenseService.filtersiteExpense(data);
  });
};
const useBulkuploadSiteExpanse = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.createsiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};
export {
  useGetAllsiteExpense,
  useGetBysiteExpenseID,
  useCreatesiteExpense,
  useUpdatesiteExpense,
  useDeletesiteExpense,
  useGetBySearchsiteExpense,
  useBulkuploadSiteExpanse,
};
