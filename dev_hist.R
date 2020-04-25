covid_data %>% 
  dplyr::filter(resultado == "Positivo SARS-CoV-2") %>% 
  dplyr::filter(fecha_def != "9999-99-99") %>% 
  dplyr::select(edad) %>% 
  ggplot2::ggplot(.)+
  ggplot2::geom_bar(ggplot2::aes(edad))
  
nrow(covid_data[covid_data$resultado == "Positivo SARS-CoV-2"&covid_data$fecha_def != "9999-99-99",])

covid_data %>% 
  dplyr::filter(resultado == "Positivo SARS-CoV-2") %>% 
  dplyr::filter(fecha_def != "9999-99-99") %>% 
  dplyr::select(fecha_def) %>% 
  table() %>% 
  as.data.frame() %>% 
  dplyr::rename(., fecha = ., freq = Freq) %>% 
  ggplot2::ggplot(.)+
  ggplot2::geom_line(ggplot2::aes(lubridate::ymd(fecha), freq))

covid_data %>% 
  dplyr::filter(resultado == "Positivo SARS-CoV-2") %>% 
  dplyr::filter(fecha_def != "9999-99-99") %>% 
  dplyr::select(fecha_def) %>% 
  table() %>% 
  as.data.frame() %>% 
  dplyr::rename(., fecha = ., freq = Freq) %>% 
  dplyr::mutate(acum = cumsum(freq)) %>% 
  ggplot2::ggplot(.)+
  ggplot2::geom_line(ggplot2::aes(lubridate::ymd(fecha), acum))+
  ggplot2::geom_line(ggplot2::aes(lubridate::ymd(fecha), freq))

a <- covid_data %>% 
  dplyr::filter(resultado == "Positivo SARS-CoV-2") %>% 
  dplyr::filter(entidad_res == "CIUDAD DE MÉXICO") %>% 
  dplyr::group_by(municipio_res) %>% 
  dplyr::summarise(casos = n()) %>% 
  dplyr::mutate(casos_estimados = casos*8.885342226)

b <- covid_data %>% 
  dplyr::filter(resultado == "Positivo SARS-CoV-2") %>% 
  dplyr::filter(entidad_res == "CIUDAD DE MÉXICO") %>% 
  dplyr::filter(fecha_def != "9999-99-99") %>% 
  dplyr::group_by(municipio_res) %>% 
  dplyr::summarise(defunciones = n())

dplyr::right_join(a,b)
