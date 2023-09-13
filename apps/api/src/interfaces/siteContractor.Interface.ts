interface createSiteContractorBody {
  name: string;
  type: string;
  mobile_number: string;
  contact_number: string;
  address: JSON;
  description: string;
  created_by: number;
  code: string;
}

interface updateSiteContractorBody {
  site_contractor_id: number;
  name: string;
  type: string;
  mobile_number: string;
  contact_number: string;
  address: JSON;
  description: string;
  updated_by: number;
  code: string;
}

export { createSiteContractorBody, updateSiteContractorBody };
