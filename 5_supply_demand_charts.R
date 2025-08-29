
pce$quantity


pce %>%
  filter(year(date) >= 2023) %>%
  filter(LineDescription == "Goods") %>%
  ggplot(aes(quantity, Value)) +
  geom_path()

pce %>%
  filter(year(date) >= 2023) %>%
  filter(LineDescription == "Durable goods") %>%
  ggplot(aes(quantity, Value)) +
  geom_path() +
  theme_esp() +
  labs(y = "Quantity", x = "Price")

pce %>%
  filter(year(date) >= 2025) %>%
  filter(LineDescription == "PCE services excluding energy") %>%
  ggplot(aes(quantity, Value)) +
  geom_path()