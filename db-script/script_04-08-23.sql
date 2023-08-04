alter table site 
add column type varchar(20) ;

alter table site 
add column mobile_number varchar(20) ;

alter table site 
add column address json;

alter table site 
add column description varchar(300);