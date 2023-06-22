library(tidyverse)
data(mpg)

# Creating a ggplot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy))

# Exercise 1
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

nrow(mtcars)
ncol(mtcars)

help(mpg)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

# Aesthetic Mappings
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy, size = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy, alpha = class))


# Exercise 2
# Q2
?mpg

# Q3
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy, color = factor(cty), size = cty))
# A continuous variable cannot be mapped to shape
# Categorical variable gives different color, size and shape to each category, continuous variable provides a range (like lighter to dark or smaller to bigger size)

# Q4
# Mapping the same variable to multiple aesthetics works and shows different aesthetics in the plot

# Q5
?geom_point
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy), stroke = 3)

# Q6
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty,y = hwy, color = displ < 5))

# Facets
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy)) +
  facet_wrap( ~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy)) +
  facet_grid(. ~ cyl)

# Exercises 3
# Q1

# Q2


# Geometric Objects
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ,y = hwy, linetype = drv))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ,y = hwy, color = drv),
              show.legend = FALSE)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ,y = hwy))

ggplot(data = mpg, mapping = aes(displ, hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(displ, hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(
    data = filter(mpg, class == "subcompact"),
    se = FALSE
  )

# Exercise 4
# Q1

# Q2
ggplot(
  data = mpg,
  mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point() +
  geom_smooth(se = FALSE)

# Q6
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth(mapping = aes(group = drv), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv)) + 
  geom_smooth(mapping = aes(linetype = drv),se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv))

# Statistical transformations
?stat_bin
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
  )

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max, 
    fun.y = median
  )

# Exercise 5


# Position Adjustments
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, color = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# position = "identity"
ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
) +
  geom_bar(fill = NA, position = "identity")

# position = "fill"
ggplot(data = diamonds, 
       mapping = aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill")

# position = "dodge"
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), 
           position = "dodge")

# position = "jitter"
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),
             position = "jitter")

# Exercise 6
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(mapping = aes(color = drv))

?geom_jitter

# Coordinate systems
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() + 
  coord_flip()

# Exercise 7

# The Layered Grammar of Graphics

"ggplot(data = <DATA>) +
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS),
    stat = <STAT>, 
    position = <POSITION>,
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>"


