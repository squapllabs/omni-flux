alter table category 
add column progress_status varchar(50);

alter table project_inventory 
add column site_id int4 ;

alter table project_inventory 
add constraint fk_project_inventory_site_id foreign key (site_id) references site_contractor(site_contractor_id);

