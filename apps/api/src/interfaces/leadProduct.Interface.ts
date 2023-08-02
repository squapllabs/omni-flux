interface createLeadProductBody {
  lead_enquiry_id: number;
  source_name: string;
  probability: number;
  our_remarks: string;
  client_remark: string;
  approx_value: number;
  sales_person_name: number;
  created_by: number;
}

interface updateLeadProductBody {
  lead_product_id: number;
  lead_enquiry_id: number;
  source_name: string;
  probability: number;
  our_remarks: string;
  client_remark: string;
  approx_value: number;
  sales_person_name: number;
  updated_by: number;
}

export { createLeadProductBody, updateLeadProductBody };
