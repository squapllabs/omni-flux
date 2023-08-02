interface createLeadEnquiryBody {
  /* Enquiry */
  lead_type: string;
  lead_code: string;
  client: number;
  client_level: number;
  client_contact_name: string;
  client_contact_email: string;
  client_contact_phone: string;
  our_remarks: string;
  client_remark: string;
  doc_url: string;
  created_by: number;
  status_remarks: string;
  /* Product */
  source_name: string;
  status: string;
  lead_enquiry_id: string;
  probability: number;
  approx_value: number;
  sales_person_name: number;
  /* Tender */
  tender_reg_no: string;
  tender_identification_no: string;
  tender_name: string;
  tender_issue_date: Date;
  tender_due_date: Date;
  tender_type: string;
  estimated_value: number;
  industry_sector: number;
  /* Product Item */
  product_id: number;
  quantity: number;
}

interface updateLeadEnquiryBody {
  /* Enquiry */
  lead_type: string;
  lead_code: string;
  client: number;
  client_level: number;
  client_contact_name: string;
  client_contact_email: string;
  client_contact_phone: string;
  our_remarks: string;
  client_remark: string;
  doc_url: string;
  status_remarks: string;
  updated_by: number;
  /* Product */
  source_name: string;
  status: string;
  lead_enquiry_id: number;
  probability: number;
  approx_value: number;
  sales_person_name: number;
  /* Tender */
  tender_reg_no: string;
  tender_identification_no: string;
  tender_name: string;
  tender_issue_date: Date;
  tender_due_date: Date;
  tender_type: string;
  estimated_value: number;
  industry_sector: number;
  /* Product Item */
  product_id: number;
  quantity: number;
  lead_product_id: number;
  lead_tender_id: number;
  lead_enquiry_product_item_id: number;
}

export { createLeadEnquiryBody, updateLeadEnquiryBody };
