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
  project_type: string;
  project_notes: string;
  site_configuration: Array<SiteConfiguration>;
  approvar_id: number;
}

interface SiteConfiguration {
  site_id: number;
  status: string;
  is_delete: string;
  estimated_budget: number;
  actual_budget: number;
  approvar_id: number;
  project_site_id: number;
}

interface ProjectDocuments {
  index: number;
  path: string;
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
  project_type: string;
  project_notes: string;
  client_id: number;
  project_documents: Array<ProjectDocuments>;
  updated_by: bigint;
  site_configuration: Array<SiteConfiguration>;
  approvar_id: number;
}

export { createProjectBody, updateProjectBody, ProjectDocuments };
