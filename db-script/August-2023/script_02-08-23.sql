alter table lead_enquiry_tender drop constraint pk_lead_tender;

alter table lead_enquiry_tender 
add constraint pk_lead_tender primary key (lead_tender_id);

create table project_workbreak_down(
project_workbreak_down_id serial4 not null,
project_workbreak_down_name varchar(100) not null,
project_workbreak_down_description varchar(250),
project_workbreak_down_code varchar(20),
parent_project_workbreak_down_id int4,
rate double precision,
uom_id int4 ,
project_workbreak_down_type varchar(20) not null,
project_id int4,
site_id int4,
is_delete boolean not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8 ,
updated_by int8,
constraint pk_project_workbreak_down primary key (project_workbreak_down_id),
constraint fk_project_workbreak_down_parent_project_workbreak_down_id foreign key (parent_project_workbreak_down_id) references project_workbreak_down(project_workbreak_down_id),
constraint fk_project_workbreak_down_uom_id foreign key (uom_id) references uom(uom_id),
constraint fk_project_workbreak_down_project_id foreign key (project_id) references project(project_id),
constraint fk_project_workbreak_down_site_id foreign key (site_id) references site(site_id)
)
