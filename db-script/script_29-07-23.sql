CREATE TABLE public.lead_enquiry (
	lead_enquiry_id serial4 NOT NULL,
	lead_type varchar(50) NULL,
	lead_code varchar(20) NULL,
	client_id int8 NULL,
	client_level int8 NULL,
	client_contact_name varchar(100) NULL,
	client_contact_email varchar(100) NULL,
	client_contact_phone varchar(20) NULL,
	doc_url varchar(500) NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_lead_enquiry PRIMARY KEY (lead_enquiry_id)
);

CREATE TABLE public.lead_product (
	lead_product_id serial4 NOT NULL,
	lead_enquiry_id int4 NULL,
	source_name varchar(100) NOT NULL,
	probability varchar(50) NULL,
	our_remarks varchar(100) NULL,
	client_remark varchar(100) NULL,
	approx_value float8 NULL,
	sales_person_name int8 NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_lead_product PRIMARY KEY (lead_product_id)
);

CREATE TABLE public.lead_tender (
	lead_tender_id serial4 NOT NULL,
	lead_enquiry_id int4 NULL,
	approx_value float8 NULL,
	sales_person_name int8 NULL,
	tender_reg_no varchar(50) NULL,
	tender_identification_no varchar(50) NULL,
	tender_name varchar(50) NULL,
	tender_issue_date timestamptz NULL,
	tender_due_date timestamptz NULL,
	tender_type varchar(50) NULL,
	estimated_value float8 NULL,
	industry_sector varchar(200) NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL,
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_lead_tender PRIMARY KEY (lead_tender_id)
);
