create table cart_users (
  email      varchar not null primary key,
  first_name varchar not null,
  last_name  varchar not null,
  is_member   boolean not null,
  days_in_queue   integer not null
);