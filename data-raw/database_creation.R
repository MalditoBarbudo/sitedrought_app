
# /////////////////////////////  SITEDROUGHT  ///////////////////////////
# ///////////////////////////////////////////////////////////////////////


# %%%%%%%%%%%%%%%%    CREAR BBDD PRINCIPAL POSTGRESQL  %%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#     .) CONECTAR a la BBDD principal (POSGRESQL)
#     .) CREAR DE NUEVO la BBDD (sitedrought_db)
#     .) La nueva BBDD usarà el SUPERUSARIO (postgres)


conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = 'ifn',
  host = 'laboratoriforestal.creaf.cat',
  port = 5432,
  user = 'ifn',
  password = keyring::key_get('lfc_postgres_admin', keyring = 'malditobarbudo')
)

# create database and activate postgis
drop_database <- glue::glue_sql(
  .con = conn,
  "DROP DATABASE IF EXISTS sitedrought_db ;"
)

sql_table_creation_1 <- glue::glue_sql(
  .con = conn,
  "
  CREATE DATABASE sitedrought_db;
  "
)
sql_table_creation_2 <- glue::glue_sql(
  .con = conn,
  "
  GRANT ALL PRIVILEGES ON DATABASE sitedrought_db TO ifn;
  "
)
sql_table_creation_3 <- glue::glue_sql(
  .con = conn,
  "
  GRANT CONNECT ON DATABASE sitedrought_db TO guest;
  "
)

RPostgres::dbExecute(conn, drop_database)
RPostgres::dbExecute(conn, sql_table_creation_1)
RPostgres::dbExecute(conn, sql_table_creation_2)
RPostgres::dbExecute(conn, sql_table_creation_3)

# change conn
RPostgres::dbDisconnect(conn)
conn <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = 'sitedrought_db',
  host = 'laboratoriforestal.creaf.cat',
  port = 5432,
  user = 'ifn',
  password = keyring::key_get('lfc_postgres_admin', keyring = 'malditobarbudo')
)

sql_guest_activation_1 <- glue::glue_sql(
  "
  GRANT USAGE ON SCHEMA public TO guest;
  "
)
sql_guest_activation_2 <- glue::glue_sql(
  "
  GRANT SELECT ON ALL TABLES IN SCHEMA public TO guest;
  "
)
sql_guest_activation_3 <- glue::glue_sql(
  "
  ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT ON TABLES TO guest;
  "
)

RPostgres::dbExecute(conn, sql_guest_activation_1)
RPostgres::dbExecute(conn, sql_guest_activation_2)
RPostgres::dbExecute(conn, sql_guest_activation_3)

# postgis extension
rpostgis::pgPostGIS(conn, topology = TRUE, sfcgal = TRUE)


# %%%%%%%%%%%%%%%%%%%%%%%%   CREAR GEOMETRÍA  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#      .) MEDFATE cada día crea los RDS con la info de cada PLOT
#          .) Cada día se crean 5316 archivos RDS del MEDFATE
#          .) Cada archivo RDS es UNA PARCELA
#          .) Cada PARCELA => tiene los 365 días de datos del WHATER BALANCE

#          .) En esta PRUEVA DE APP
#          .) Tengo un FOLDER en el LOCALHOST con 5316 RDS 
#          .) Después en el servidor, el Script, también buscará en una carpeta 
#          .) En esta carpeta habrá descargadas las 5316 y algo parcelas cada día

#      .) Pero NO OFRECE la geometría de cada plot
#      .) La obtenemos de un SHAPE
#      .) plots_geom_all PLOTS_GEOM_ALL.shp




# ........... CREACIÓN de SF ............
# .......................................

#     .) Necesitamos los 4 SF
#     .) Los SF tienen orígenes diferentes

#              .) PLOTS PERE          (Miquel Shape) 
#              .) PLOTS AIGUESTORTES  (Miquel TXT)
#              .) PLOTS ORDESSA       (Miquel TXT)
#              .) IFN4                (App NFI)

# ....... SF = PLOTS PERE ..
# ..........................

pere_sf <- sf::st_read('SHAPES/PLOTS/coords_topo.shp') %>% 
  dplyr::mutate(plot_id = paste0("S_",Id), geom = geometry) %>%
  dplyr::select(plot_id, geom)  %>% 
  sf::st_transform(crs = 4326)


# ....... SF = PLOTS AiguesTortes
# ..............................

aiguestortes_csv <- read.csv2('SHAPES/PLOTS/plots_AiguesTortes.csv') %>%
  dplyr::mutate(longitude = x, latitude = y) %>%
  dplyr::select(plot_id, longitude, latitude)


aiguestortes_sf <- sf::st_as_sf(aiguestortes_csv, coords = c("longitude", "latitude"), 
                            crs = 25831, agr = "constant") %>% sf::st_transform(crs = 4326) %>%
  dplyr::mutate(geom = geometry)

# ....... SF =PLOTS ORDESA.....
# .............................

ordesa_csv <- read.csv2('SHAPES/PLOTS/plots_Ordesa_perimetre.csv') %>%
  dplyr::mutate(longitude = x, latitude = y) %>%
  dplyr::select(plot_id, longitude, latitude)


ordesa_sf <- sf::st_as_sf(ordesa_csv, coords = c("longitude", "latitude"), 
                      crs = 25830, agr = "constant") %>% sf::st_transform(crs = 4326) %>%
  dplyr::mutate(geom = geometry)


# ....... SF = IFN4 ........... 
# .............................

#     .) La geometría de los IFN4 Catalunya
#     .) La tengo en la BBDD del LOCALHOST

nfi4_sf <- lfcdata::nfi()$get_data('plots', spatial = TRUE) |>
  dplyr::filter(presence_NFI_4) |>
  dplyr::select(plot_id)

# con <- DBI::dbConnect(RPostgres::Postgres(),
#                       dbname = 'catalunya', 
#                       host = 'localhost',  
#                       port = 5432,  
#                       user = 'postgres',
#                       password = '12345database')
# 
# parcelas_nfi <- sf::st_read(dsn = con, Id(schema="creaf", table = "nif_app"))
# 
# RPostgres::dbDisconnect(con)


# ........... CREACIÓN SHAPE ............
# .......................................

#     .) Los GUARDO los 4 SF en el PC como shapes
#     .) Después usando QGIS uniré los 4 shapes en 1 shape
#     .) Uso QGIS para unir los 4 shapes

#     .) Creo un SHAPE con la geometría de TODOS los plots
#     .) PLOTS_GEOM_ALL.shp



# st_write(parcelas_nfi,'SHAPE/PLOTS/SEPARADOS/polts_nfi.shp')
# st_write(aiguestortes_sf,'SHAPE/PLOTS/SEPARADOS/polts_at.shp')
# st_write(pere_sf,'SHAPE/PLOTS/SEPARADOS/polts_p.shp')
# st_write(ordesa_sf,'SHAPE/PLOTS/SEPARADOS/polts_o.shp')

dplyr::bind_rows(
  aiguestortes_sf, ordesa_sf, pere_sf, nfi4_sf
) |>
  dplyr::select(-geom) |>
  sf::st_write(conn, "plots_coordinates")


RPostgres::dbDisconnect(conn)

