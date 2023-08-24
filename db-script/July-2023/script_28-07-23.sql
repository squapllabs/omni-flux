
CREATE TABLE master_data (
	master_data_id serial4 NOT NULL,
	master_data_name varchar(100) NOT NULL,
	master_data_description varchar(250) NULL,
	master_data_type varchar(20) NOT NULL,
	parent_master_data_id int4 NULL,
	is_delete bool NOT NULL DEFAULT false,
	created_date timestamptz NOT NULL DEFAULT now(),
	updated_date timestamptz NOT NULL DEFAULT now(),
	created_by int8 NULL,
	updated_by int8 NULL,
	CONSTRAINT pk_master_data PRIMARY KEY (master_data_id),
	CONSTRAINT fk_master_data_parent_master_data_id FOREIGN KEY (parent_master_data_id) REFERENCES master_data(master_data_id)
);

alter table master_data 
add constraint uk_master_data_parent_master_data_id_and_master_data_type unique(master_data_type,parent_master_data_id)
