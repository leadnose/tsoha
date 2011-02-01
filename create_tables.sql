create table recipe (
    id serial primary key not null,
    name varchar(255) not null,
    instructions text not null
);

create table ingredient (
    id serial primary key not null,
    name varchar(255) not null
);

create table unit (
       name varchar(50) primary key not null
);

create table recipe_ingredient_unit_amount (
    recipe_id serial references recipe(id),
    ingredient_id serial references ingredient(id),
    unit_name varchar(50) references unit(name),
    amount float not null
);

create table nutrition (
    name varchar(50) primary key not null,
    calories_per_100g float not null
);


create table ingredient_nutrition (
    ingredient_id serial references ingredient(id),
    nutrition_name varchar(50) references nutrition(name),
    grams_per_100g float not null
);

create table unit_conversion (
    unit_from varchar(50) references unit(name),
    unit_to varchar(50) references unit(name)
);