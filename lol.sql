select ingredient.name, recipe_ingredient_unit_amount.amount, unit.name
from recipe, ingredient
inner join recipe_ingredient_unit_amount
on recipe_ingredient_unit_amount.recipe_id = recipe.id ;