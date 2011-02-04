create table recipe (
    id serial primary key ,
    name varchar(255) not null,
    instructions text not null
);

create table ingredient (
    name varchar(255) primary key 
);

create table unit (
       name varchar(50) primary key 
);

create table recipe_ingredient_unit_amount (
    recipe_id serial references recipe(id),
    ingredient_name varchar(255) references ingredient(name),
    amount float not null,
    unit_name varchar(50) references unit(name)
);

create table nutrition (
    name varchar(50) primary key,
    calories_per_100g float not null
);


create table ingredient_nutrition (
    ingredient_name varchar(255) references ingredient(name),
    nutrition_name varchar(50) references nutrition(name),
    grams_per_100g float not null
);

create table unit_conversion (
    unit_from varchar(50) references unit(name),
    unit_to varchar(50) references unit(name)
);
