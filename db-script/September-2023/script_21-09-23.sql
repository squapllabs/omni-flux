create table stock_audit(
stock_audit_id serial4 not null,
project_id int4 not null,
site_id int4,
stock_audit_date date,
item_details jsonb,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int4,
updated_by int4,
constraint pk_stock_audit primary key (stock_audit_id),
constraint fk_stock_audit_project_id foreign key (project_id) references project(project_id),
constraint fk_stock_audit_site_id foreign key (site_id) references site_contractor(site_contractor_id)
)
