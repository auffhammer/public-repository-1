rm(list = ls()) # clear memory
setwd("/Users/auffhammer/tmp/COMPSS212_F24/slides/Lecture_06")
options(htmltools.dir.version = FALSE)
library(pacman)
p_load(MatchIt, cobalt)
View(lalonde)
table(lalonde$treat)
t.test(re78 ~ treat,data=lalonde)

exact_match <- matchit(treat ~ age + educ + race + married + nodegree, exact = ~ age + educ + race + married + nodegree, data = lalonde)
exact_match
ps_match <- matchit(treat ~ age + educ + race + married + nodegree, data = lalonde)
ps_match
love.plot(exact_match, drop.distance = TRUE)
love.plot(ps_match, drop.distance = TRUE)

matched <- match.data(ps_match)
View(matched)
t.test(re78 ~ treat, data = matched)