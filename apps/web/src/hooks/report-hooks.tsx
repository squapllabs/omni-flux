import { useQuery, useMutation, useQueryClient } from 'react-query';
import ReportService from '../service/report-service';

const usePurchaseRegisterReportData = () => {
  const queryClient = useQueryClient();
  return useMutation((data: any) => {
    return ReportService.getPurchaseRegisterReport(data);
  });
};

export { usePurchaseRegisterReportData };
