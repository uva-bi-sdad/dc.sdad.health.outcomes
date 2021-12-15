
# connect to database function
get_db_conn <-
  function(db_name = Sys.getenv("db_nam"),
           db_host = Sys.getenv("db_hst"),
           db_port = Sys.getenv("db_prt"),
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }


# write to data commons database, auto-detect if writing geom or not, change owner to data_commons
dc_dbWriteTable <-
  function(
    db_conn,
    schema_name,
    table_name,
    table_data,
    table_owner = "data_commons"
  ) {
    # check for geometry/geography columns
    tf <- sapply(table_data, {function(x) inherits(x, 'sfc')})
    # if TRUE, use sf
    if (TRUE %in% tf) {
      sf_write_result <-
        sf::st_write(
          obj = table_data,
          dsn = db_conn,
          layer = c(schema_name, table_name),
          row.names = FALSE,
          delete_layer = TRUE
        )
      print(sf_write_result)
      # if FALSE, use DBI
    } else {
      write_result <-
        DBI::dbWriteTable(
          conn = db_conn,
          name = c(schema_name, table_name),
          value = table_data,
          row.names = FALSE,
          overwrite = TRUE
        )
      print(write_result)
    }
    # change table owner
    chgown_result <- DBI::dbSendQuery(conn = db_conn, statement = paste0("ALTER TABLE ", schema_name, ".", table_name, " OWNER TO ", table_owner))
    print(chgown_result)
  }
