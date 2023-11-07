import { useQuery, useMutation, useQueryClient } from 'react-query';
import ReportService from '../service/report-service';

const purchaseRegisterReportData = () => {
    const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ReportService.getPurchaseRegisterReport(data);
    },
    {
      onSuccess: (response) => {
        console.log("response------>",response);
        
        response;
      },
    }
  );
};

export {
    purchaseRegisterReportData
}