# Carregue a biblioteca ggvenn
library(ggvenn)

# Defina as cores do diagrama
myCol = c("#f9e78a", "#81eb84")

# Prepare os conjuntos removendo valores NA
SON = na.omit(Tabela_para_diagrama$SON)
NIL = na.omit(Tabela_para_diagrama$NIL)

# Organize os conjuntos em uma lista nomeada
X <- list(
  SON = SON,
  NIL = NIL
)

# Crie o diagrama de Venn com as configurações desejadas
venn_plot <- ggvenn(
  X, 
  fill_color = myCol,
  stroke_size = 0,               # Remove as bordas
  show_percentage = FALSE,
  text_size = 12,                  # Ajusta o tamanho do texto
  set_name_size = 0,              # Ajusta o tamanho dos nomes dos conjuntos
  fill_alpha = 0.3,               # Define a transparência do preenchimento
  auto_scale = TRUE
) + theme_void() +                # Remove o fundo
  annotate("text", x = 0, y = 1.2, label = "SON (258)                 NIL (313)", size = 0, family = "Arial", fontface = "bold") +
  theme(
    text = element_text(family = "Arial"),   # Define a fonte como Arial
    panel.border = element_blank(),          # Remove borda do painel
    plot.background = element_blank()        # Remove fundo da plotagem
  )


# Exibir o gráfico
print(venn_plot)
ggsave("Resultados lipidômica/VennDiagram/Empty_Male_VennDiagram.tiff", width = 3, height = 3)

