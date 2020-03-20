library (dplyr)
library (ggplot2)
library (knitr)
library (knitr)
library (stringr)
library (DT)
library (data.table)

#import dataset
orders <- fread("orders.csv")
head (orders)
products <- fread("products.csv")
order_products <- fread("order_products__train.csv")
order_products_prior <- fread("order_products__prior.csv")
aisles <- fread("aisles.csv")
departments <- fread("departments.csv")

#glimpse the dataset
kable(head(orders))
glimpse(orders)

#data preprocess
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day),
                            eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

#data visualization
#time of order
orders %>%
  ggplot(aes(x=order_hour_of_day)) +
  geom_histogram(stat = "count",fill = "green")

#day of week
orders %>% 
  ggplot(aes(x=order_dow)) +
  geom_histogram(stat = "count", fill = "green")

#day of re-order
orders %>%
  ggplot(aes(x=days_since_prior_order)) +
  geom_histogram(stat = "count",fill = "green")

#number of prior orders
orders %>% filter(eval_set == "prior") %>% count(order_number) %>% ggplot(
  aes(order_number,n)) + geom_line(color = "green",size = 1) + geom_point(
    size = 2, color = "green")

#number of items people buy
order_products %>%
  group_by(order_id) %>%
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x = n_items))+
  geom_histogram(stat = "count",fill = "green") +
  geom_rug() +
  coord_cartesian(xlim = c(0,80))

#bestsellers
bestseller <- order_products %>%
  group_by(product_id) %>%
  summarize(count = n()) %>%
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name), by ="product_id") %>%
  arrange(desc(count))
kable(bestseller)

bestseller %>%
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="green")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

#most often reordered
reordered <-order_products %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
  filter(n>40) %>% 
  top_n(10,wt=proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")

kable(reordered)

reordered %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="green")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

#association between time of last order and probability of reorder
order_products %>% 
  left_join(orders,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="green")

#organic vs non-organic
products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))




