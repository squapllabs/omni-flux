interface createSiteContractorBody {
  name: string;
  type: string;
  mobile_number: string;
  contact_number: string;
  address: JSON;
  description: string;
  created_by: number;
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
}

export { createSiteContractorBody, updateSiteContractorBody };
