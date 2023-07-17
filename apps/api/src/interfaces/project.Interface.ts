interface createProjectBody {
  project_name: string;
  description: string;
  user_id: number;
  date_started: Date;
  date_ended: Date;
  status: string;
  budget: number;
  client_id: number;
  document_url: string;
  created_by: bigint;
}

interface updateProjectBody {
  project_id: number;
  project_name: string;
  description: string;
  user_id: number;
  date_started: Date;
  date_ended: Date;
  status: string;
  budget: number;
  client_id: number;
  document_url: string;
  updated_by: bigint;
}

export { createProjectBody, updateProjectBody };
