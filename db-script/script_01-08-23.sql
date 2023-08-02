
CREATE TABLE lead_enquiry (
    lead_enquiry_id serial4 NOT NULL,
    lead_type varchar(50),--PRODUCT,TENDER
    lead_code varchar(20),--sys generated id
    client int4,
    client_level int4,
    client_contact_name varchar(100),
    client_contact_email varchar(100),
    client_contact_phone varchar(20),
    our_remarks varchar(1000),
    client_remark varchar(1000),
    doc_url varchar(1000),
    status varchar(20),--AWARDED,REJECTED,CLONE,COMPLETED,INPROGRESS
    is_delete bool NOT NULL DEFAULT false, 
    created_date timestamptz NOT NULL,
    updated_date timestamptz NOT NULL,
    created_by int8 NULL,
    updated_by int8 NULL,
    CONSTRAINT pk_lead_enquiry PRIMARY KEY (lead_enquiry_id),
    CONSTRAINT fk_lead_enquiry_client FOREIGN KEY (client) REFERENCES public.client(client_id),
    CONSTRAINT fk_lead_enquiry_client_level FOREIGN KEY (client_level) REFERENCES public.master_data(master_data_id)
);


CREATE TABLE lead_enquiry_product (
    lead_product_id serial4 NOT NULL,
    lead_enquiry_id int4,
    source_name VARCHAR(100) NOT NULL,
    probability int4,
    approx_value double precision,
    sales_person_name int4,
    is_delete bool NOT NULL DEFAULT false,
    created_date timestamptz NOT NULL,
    updated_date timestamptz NOT NULL,
    created_by int8 NULL,
    updated_by int8 NULL,
    CONSTRAINT pk_lead_product PRIMARY KEY (lead_product_id),
    CONSTRAINT fk_lead_product_lead_enquiry FOREIGN KEY (lead_enquiry_id) REFERENCES public.lead_enquiry(lead_enquiry_id),
    CONSTRAINT fk_lead_enquiry_product_user FOREIGN KEY (sales_person_name) REFERENCES public.users(user_id),
    CONSTRAINT fk_lead_enquiry_product_master_data FOREIGN KEY (probability) REFERENCES public.master_data(master_data_id)
);

CREATE TABLE lead_enquiry_tender (
    lead_tender_id serial4 NOT NULL,
    lead_enquiry_id int4,
    approx_value double precision,
    tender_reg_no varchar(50),
    tender_identification_no varchar(50),
    tender_name varchar(100),
    tender_issue_date date,
    tender_due_date date,
    tender_type varchar(50),
    estimated_value double precision,
    industry_sector int4,
    is_delete bool NOT NULL DEFAULT false,
    created_date timestamptz NOT NULL,
    updated_date timestamptz NOT NULL,
    created_by int8 NULL,
    updated_by int8 NULL,
    CONSTRAINT pk_lead_tender PRIMARY KEY (lead_enquiry_id),
    CONSTRAINT fk_lead_tender_lead_enquiry_id FOREIGN KEY (lead_enquiry_id) REFERENCES public.lead_enquiry(lead_enquiry_id),
    CONSTRAINT fk_lead_enquiry_tender_industry_sector FOREIGN KEY (industry_sector) REFERENCES public.master_data(master_data_id)
);

CREATE TABLE lead_enquiry_product_item (
	lead_enquiry_product_item_id serial4 not null,
    lead_enquiry_id int4,
    product_id int4,
    quantity int,
    created_date timestamptz NOT NULL,
    updated_date timestamptz NOT NULL,
    created_by int8 NULL,
    updated_by int8 NULL,
    constraint pk_lead_enquiry_product_item_id primary key (lead_enquiry_product_item_id),
    CONSTRAINT fk_lead_enquiry_product_item_lead_enquiry_id FOREIGN KEY (lead_enquiry_id) REFERENCES public.lead_enquiry(lead_enquiry_id),
    CONSTRAINT fk_lead_enquiry_product_item_product_id FOREIGN KEY (product_id) REFERENCES public.item(item_id)
);



	