interface createLeadEnquiryBody {
  lead_type: string;
  lead_code: string;
  client_id: number;
  client_level: number;
  client_contact_name: string;
  client_contact_email: string;
  client_contact_phone: string;
  doc_url: string;
  created_by: number;
  source_name: string;
  probability: string;
  our_remarks: string;
  client_remark: string;
  approx_value: number;
  sales_person_name: number;
  tender_reg_no: string;
  tender_identification_no: string;
  tender_name: string;
  tender_issue_date: Date;
  tender_due_date: Date;
  tender_type: string;
  estimated_value: number;
  industry_sector: string;
}

interface updateLeadEnquiryBody {
  lead_enquiry_id: number;
  lead_type: string;
  lead_code: string;
  client_id: number;
  client_level: number;
  client_contact_name: string;
  client_contact_email: string;
  client_contact_phone: string;
  doc_url: string;
  updated_by: number;
  source_name: string;
  probability: string;
  our_remarks: string;
  client_remark: string;
  approx_value: number;
  sales_person_name: number;
  tender_reg_no: string;
  tender_identification_no: string;
  tender_name: string;
  tender_issue_date: Date;
  tender_due_date: Date;
  tender_type: string;
  estimated_value: number;
  industry_sector: string;
  lead_product_id: number;
  lead_tender_id: number;
}

export { createLeadEnquiryBody, updateLeadEnquiryBody };
