import { useMutation } from 'react-query';
import ReportService from '../service/report-service';

const usePurchaseRegisterReportData = () => {
  return useMutation((data: any) => {
    return ReportService.getPurchaseRegisterReport(data);
  });
};

export { usePurchaseRegisterReportData };
