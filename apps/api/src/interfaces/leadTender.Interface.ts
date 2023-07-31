interface createLeadTenderBody {
  lead_enquiry_id: number;
  approx_value: number;
  source_name: string;
  sales_person_name: number;
  tender_reg_no: string;
  tender_identification_no: string;
  tender_name: string;
  tender_issue_date: Date;
  tender_due_date: Date;
  tender_type: string;
  estimated_value: number;
  industry_sector: string;
  created_by: number;
}

interface updateLeadTenderBody {
  lead_enquiry_id: number;
  approx_value: number;
  source_name: string;
  sales_person_name: number;
  tender_reg_no: string;
  tender_identification_no: string;
  tender_name: string;
  tender_issue_date: Date;
  tender_due_date: Date;
  tender_type: string;
  estimated_value: number;
  industry_sector: string;
  updated_by: number;
  lead_tender_id: number;
}

export { createLeadTenderBody, updateLeadTenderBody };
