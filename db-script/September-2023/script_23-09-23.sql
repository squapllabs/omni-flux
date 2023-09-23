alter table category 
add column progress_status varchar(50);

alter table project_inventory 
add column site_id int4 ;

alter table project_inventory 
add constraint fk_project_inventory_site_id foreign key (site_id) references site_contractor(site_contractor_id);

CREATE TABLE expense (
	expense_id serial4 NOT NULL,
	expense_code varchar(100) not null,
	site_id int4 NULL,
	project_id int4 NULL,
	employee_name varchar(50) NULL,
	employee_id varchar(20) NULL,
	employee_phone varchar(20) NULL,
	purpose varchar(50) NULL,
	department varchar(50) NULL,
	designation varchar(50) NULL,
	start_date date NULL,
	end_date date NULL,
	bill_details jsonb ,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL DEFAULT now(),
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_expense PRIMARY KEY (expense_id),
	constraint uk_expense_expense_code unique(expense_code),
	constraint fk_expense_site_id foreign key (site_id) references site_contractor(site_contractor_id),
	constraint fk_expense_project_id foreign key (project_id) references project(project_id)
);

create table expense_details(
	expense_details_id serial4 NOT NULL,
	expense_id int4 NOT NULL,
	expense_data_id int4 ,
	total float8 NULL,
	bill_details jsonb NULL,
	created_date timestamptz NOT NULL DEFAULT now(),
	updated_date timestamptz NOT NULL,
	created_by int8 NULL,
	updated_by int8 NULL,
	status varchar(50) NULL,
	comments varchar(300) NULL,
	progressed_date timestamptz NULL,
	progressed_by int4 NULL,
	is_delete bool NOT NULL DEFAULT false,
	CONSTRAINT pk_expense_details PRIMARY KEY (expense_details_id),
	constraint fk_expense_details_expense_id foreign key (expense_id) references expense(expense_id),
	constraint fk_expense_details_expense_data_id foreign key (expense_data_id) references master_data(master_data_id)
)

create sequence expence_code_sequence
start 1
increment 1;

select concat('EXP',DATE_PART('year', CURRENT_DATE),'00',nextval('expence_code_sequence')::text) as expence_code_sequence;

alter table expense_details 
add column bill_number varchar(20);