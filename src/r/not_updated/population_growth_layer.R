# this file generate a layer with the population growth of each
# census block.
# the values are obtained from the inter cencus survey  (INEGI)
# source:
# http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/inter_censal/estados2015/702825079741.pdf

for (i in 1:(length(clave_municipalities$ID) - 1)) {
  agebs_indel <- which(as.character(studyArea_CVG@data$municipio) == as.character(clave_municipalities$ID[i]))
  studyArea_CVG@data$pop_growth[agebs_indel] <- rep(clave_municipalities$crecimiento_poblacional[i] / 100, length(agebs_indel))
}
