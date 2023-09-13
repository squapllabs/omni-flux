alter table project_member_association 
drop constraint fk_project_member_association_project_role_id;

alter table project_member_association 
add constraint fk_project_member_association_project_role_id foreign key (project_role_id) references role(role_id);

create table store (
    store_id SERIAL4 not null,
    store_name VARCHAR(255),
    store_manager_id int4 not null,
    address jsonb,
    contact_email VARCHAR(100),
    contact_phone VARCHAR(20),
    project_id int4,
    site_id int4,
    is_delete boolean not null default false,
	created_date timestamptz not null default now(),
	updated_date timestamptz ,
	created_by int8,
	updated_by int8,
    constraint pk_store primary key (store_id),
    constraint fk_store_store_manager_id foreign key (store_manager_id) references users(user_id),
    constraint fk_store_project_id foreign key (project_id) references project(project_id),
    constraint fk_store_site_id foreign key (site_id) references site_contractor(site_contractor_id)
);
