interface createProjectBody {
  project_name: string;
  description: string;
  user_id: number;
  date_started: Date;
  date_ended: Date;
  status: string;
  client_id: number;
  project_documents: JSON;
  created_by: bigint;
  estimated_budget: number;
  actual_budget: number;
  code: string;
  priority: string;
  project_notes: string;
  site_configuration: Array<SiteConfiguration>;
}

interface SiteConfiguration {
  site_id: number;
  status: string;
  is_delete: string;
  estimation: number;
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
  project_notes: string;
  client_id: number;
  project_documents: JSON;
  updated_by: bigint;
  site_configuration: Array<SiteConfiguration>;
}

export { createProjectBody, updateProjectBody };
