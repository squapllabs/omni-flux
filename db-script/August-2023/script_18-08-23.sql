alter table project_site 
rename column estimation to estimated_budget;

alter table project_site 
add column actual_budget double precision;