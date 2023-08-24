alter table gst 
add column is_delete boolean not null default false;

alter table uom 
add column is_delete boolean not null default false;