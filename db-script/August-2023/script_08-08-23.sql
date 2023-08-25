-- Project Table Modifications

alter table project 
drop column budget;

alter table project 
add column estimated_budget double precision;

alter table project 
add column actual_budget double precision;

alter table project 
add column code varchar(20);

alter table project 
add column currency varchar(20);

alter table project 
add column priority varchar(50);

alter table project 
add column project_notes varchar(300);

drop table project_site ;

create table project_site(
project_site_id serial4 not null,
project_id int4,
site_id int4,
constraint pk_project_site primary key (project_site_id),
constraint fk_project_site_project_id foreign key (project_id) references project(project_id),
constraint fk_project_site_site_id foreign key (site_id) references site_contractor(site_contractor_id)
)

alter table project_site 
add column status varchar(50);

alter table project_site 
add column created_date timestamptz not null default now();

alter table project_site 
add column updated_date timestamptz not null;

alter table project_site 
add column created_by int8 ;

alter table project_site 
add column updated_by int8 ;

--Lead Enquiry ,Project Work Break Down and Site Contractor Table Scripts :DDL


CREATE TABLE public.site_contractor (
	site_contractor_id int4 NOT NULL DEFAULT nextval('site_site_id_seq'::regclass),
	"name" varchar(100) NOT NULL,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	"type" varchar(20) NULL,
	mobile_number varchar(20) NULL,
	address json NULL,
	description varchar(300) NULL,
	contact_number varchar(20) NULL,
	is_delete bool NOT NULL DEFAULT false,
	code varchar(20) NULL,
	CONSTRAINT pk_site PRIMARY KEY (site_contractor_id)
);


CREATE TABLE public.lead_enquiry (
	lead_enquiry_id serial4 NOT NULL,
	lead_type varchar(50) NULL,
	lead_code varchar(20) NULL,
	client int4 NULL,
	client_level int4 NULL,
	client_contact_name varchar(100) NULL,
	client_contact_email varchar(100) NULL,
	client_contact_phone varchar(20) NULL,
	our_remarks varchar(1000) NULL,
	client_remark varchar(1000) NULL,
	doc_url varchar(1000) NULL,
	status varchar(20) NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_lead_enquiry PRIMARY KEY (lead_enquiry_id)
);

CREATE TABLE public.lead_enquiry_product (
	lead_product_id serial4 NOT NULL,
	lead_enquiry_id int4 NULL,
	source_name varchar(100) NOT NULL,
	probability int4 NULL,
	approx_value float8 NULL,
	sales_person_name int4 NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_lead_product PRIMARY KEY (lead_product_id)
);

CREATE TABLE public.lead_enquiry_product_item (
	lead_enquiry_product_item_id serial4 NOT NULL,
	product_id int4 NULL,
	quantity int4 NULL,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	lead_enquiry_product_id int4 NULL,
	CONSTRAINT pk_lead_enquiry_product_item_id PRIMARY KEY (lead_enquiry_product_item_id)
);

CREATE TABLE public.lead_enquiry_tender (
	lead_tender_id serial4 NOT NULL,
	lead_enquiry_id int4 NOT NULL,
	approx_value float8 NULL,
	tender_reg_no varchar(50) NULL,
	tender_identification_no varchar(50) NULL,
	tender_name varchar(100) NULL,
	tender_issue_date date NULL,
	tender_due_date date NULL,
	tender_type varchar(50) NULL,
	estimated_value float8 NULL,
	industry_sector int4 NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_lead_tender PRIMARY KEY (lead_tender_id)
);

CREATE TABLE public.project_workbreak_down (
	project_workbreak_down_id serial4 NOT NULL,
	project_workbreak_down_name varchar(100) NOT NULL,
	project_workbreak_down_description varchar(250) NULL,
	project_workbreak_down_code varchar(20) NULL,
	parent_project_workbreak_down_id int4 NULL,
	rate float8 NULL,
	uom_id int4 NULL,
	project_workbreak_down_type varchar(20) NOT NULL,
	project_id int4 NULL,
	site_id int4 NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL DEFAULT now(),
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_project_workbreak_down PRIMARY KEY (project_workbreak_down_id)
);

-- public.lead_enquiry foreign keys

ALTER TABLE public.lead_enquiry ADD CONSTRAINT fk_lead_enquiry_client FOREIGN KEY (client) REFERENCES public.client(client_id);
ALTER TABLE public.lead_enquiry ADD CONSTRAINT fk_lead_enquiry_client_level FOREIGN KEY (client_level) REFERENCES public.master_data(master_data_id);


-- public.lead_enquiry_product foreign keys

ALTER TABLE public.lead_enquiry_product ADD CONSTRAINT fk_lead_enquiry_product_master_data FOREIGN KEY (probability) REFERENCES public.master_data(master_data_id);
ALTER TABLE public.lead_enquiry_product ADD CONSTRAINT fk_lead_enquiry_product_user FOREIGN KEY (sales_person_name) REFERENCES public.users(user_id);
ALTER TABLE public.lead_enquiry_product ADD CONSTRAINT fk_lead_product_lead_enquiry FOREIGN KEY (lead_enquiry_id) REFERENCES public.lead_enquiry(lead_enquiry_id);


-- public.lead_enquiry_product_item foreign keys

ALTER TABLE public.lead_enquiry_product_item ADD CONSTRAINT fk_lead_enquiry_product_item_lead_enquiry_product_id FOREIGN KEY (lead_enquiry_product_id) REFERENCES public.lead_enquiry_product(lead_product_id);
ALTER TABLE public.lead_enquiry_product_item ADD CONSTRAINT fk_lead_enquiry_product_item_product_id FOREIGN KEY (product_id) REFERENCES public.item(item_id);


-- public.lead_enquiry_tender foreign keys

ALTER TABLE public.lead_enquiry_tender ADD CONSTRAINT fk_lead_enquiry_tender_industry_sector FOREIGN KEY (industry_sector) REFERENCES public.master_data(master_data_id);
ALTER TABLE public.lead_enquiry_tender ADD CONSTRAINT fk_lead_tender_lead_enquiry_id FOREIGN KEY (lead_enquiry_id) REFERENCES public.lead_enquiry(lead_enquiry_id);


-- public.project_workbreak_down foreign keys

ALTER TABLE public.project_workbreak_down ADD CONSTRAINT fk_project_workbreak_down_parent_project_workbreak_down_id FOREIGN KEY (parent_project_workbreak_down_id) REFERENCES public.project_workbreak_down(project_workbreak_down_id);
ALTER TABLE public.project_workbreak_down ADD CONSTRAINT fk_project_workbreak_down_project_id FOREIGN KEY (project_id) REFERENCES public.project(project_id);
ALTER TABLE public.project_workbreak_down ADD CONSTRAINT fk_project_workbreak_down_site_id FOREIGN KEY (site_id) REFERENCES public.site_contractor(site_contractor_id);
ALTER TABLE public.project_workbreak_down ADD CONSTRAINT fk_project_workbreak_down_uom_id FOREIGN KEY (uom_id) REFERENCES public.uom(uom_id);
