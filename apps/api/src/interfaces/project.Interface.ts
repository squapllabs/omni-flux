interface createProjectBody {
  project_name: string;
  description: string;
  user_id: number;
  date_started: Date;
  date_ended: Date;
  status: string;
  client_id: number;
  document_url: string;
  created_by: bigint;
  estimated_budget: number;
  actual_budget: number;
  code: string;
  priority: string;
  currency: string;
  project_notes: string;
  site_configuration: Array<SiteConfiguration>;
}

interface SiteConfiguration {
  site_id: number;
  status: string;
  is_delete: string;
}
interface updateProjectBody {
  project_id: number;
  project_name: string;
  description: string;
  user_id: number;
  date_started: Date;
  date_ended: Date;
  status: string;
  estimated_budget: number;
  actual_budget: number;
  code: string;
  priority: string;
  currency: string;
  project_notes: string;
  client_id: number;
  document_url: string;
  updated_by: bigint;
  site_configuration: Array<SiteConfiguration>;
}

export { createProjectBody, updateProjectBody };
