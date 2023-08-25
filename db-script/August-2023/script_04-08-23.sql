alter table site 
add column type varchar(20) ;

alter table site 
add column mobile_number varchar(20) ;

alter table site 
add column address json;

alter table site 
add column description varchar(300);

alter table site 
drop column location;

alter table site 
rename column site_name to name;

alter table site 
drop column user_id;

alter table site rename to site_contractor;

alter table site_contractor 
rename column site_id to site_contractor_id;

alter table site_contractor 
add column contact_number varchar(20);

alter table site_contractor 
add column is_delete boolean not null default false;