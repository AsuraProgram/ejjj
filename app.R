library(shiny)
library(DBI)
library(RPostgres) # CHANGED: Using Postgres driver
library(bslib)
library(DT)
library(qrcode)
library(gridExtra)
library(grid)
library(grDevices)

# ------------------------------------
# Helpers
# ------------------------------------
fmt_peso <- function(x) {
  ifelse(
    is.na(x),
    "",
    paste0("₱", formatC(x, format = "f", digits = 2, big.mark = ","))
  )
}

normalize_text <- function(x) {
  tolower(trimws(x))
}

# ------------------------------------
# 1. DB Connection (PostgreSQL)
# ------------------------------------
db_host <- Sys.getenv("DB_HOST")
db_user <- Sys.getenv("DB_USER")
db_pass <- Sys.getenv("DB_PASS")
db_name <- Sys.getenv("DB_NAME")
db_port <- Sys.getenv("DB_PORT")

# Defaults (Render usually provides these, but safe to have defaults)
if (db_host == "") db_host <- "localhost"
if (db_user == "") db_user <- "postgres"
if (db_name == "") db_name <- "sari_sari_store"
if (db_port == "") db_port <- "5432"

con <- dbConnect(
  RPostgres::Postgres(),
  host     = db_host,
  user     = db_user,
  password = db_pass,
  dbname   = db_name,
  port     = db_port
)

onStop(function() {
  try(dbDisconnect(con), silent = TRUE)
})

# ------------------------------------
# 2. UI
# ------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .app-title { font-weight: 800; letter-spacing: .2px; }
      .subtle { color: #6c757d; }
      .card { border-radius: 18px; }
      .btn { border-radius: 12px; }
    "))
  ),
  
  title = "Ejdacian Sari Sari Store",
  
  div(
    class = "mb-3",
    h1(class = "app-title", "Ejdacian Sari Sari Store")
  ),
  
  tabsetPanel(
    type = "pills",
    
    # Tab 1
    tabPanel(
      "Customers & Products",
      br(),
      fluidRow(
        column(
          6,
          card(
            card_header("Add Customer"),
            card_body(
              textInput("cust_name", "Name", ""),
              textInput("cust_nick", "Nickname (optional)", ""),
              textInput("cust_contact", "Contact No. (optional)", ""),
              textInput("cust_address", "Address (optional)", ""),
              numericInput("cust_max_credit", "Max Credit (optional)", value = 0, min = 0),
              actionButton("btn_add_customer", "Add Customer", class = "btn-primary w-100")
            )
          )
        ),
        column(
          6,
          card(
            card_header("Customers"),
            card_body(DTOutput("tbl_customers"))
          )
        )
      ),
      br(),
      fluidRow(
        column(
          6,
          card(
            card_header("Add / Update Product"),
            card_body(
              textInput("prod_name", "Product Name", ""),
              textInput("prod_cat", "Category (optional)", ""),
              numericInput("prod_price", "Unit Price", value = 1, min = 0),
              numericInput("prod_stock", "Initial Stock to Add", value = 0, min = 0),
              checkboxInput("prod_allow_credit", "Allowed for Pautang?", value = TRUE),
              actionButton("btn_add_product", "Save Product", class = "btn-primary w-100")
            )
          )
        ),
        column(
          6,
          card(
            card_header("Products"),
            card_body(DTOutput("tbl_products"))
          )
        )
      )
    ),
    
    # Tab 2
    tabPanel(
      "Pautang / Payments",
      br(),
      fluidRow(
        column(
          6,
          card(
            card_header("Record Pautang (LOAN)"),
            card_body(
              selectInput("loan_customer", "Customer", choices = c()),
              selectInput("loan_product", "Product", choices = c()),
              numericInput("loan_qty", "Quantity", value = 1, min = 0),
              dateInput("loan_date", "Date", value = Sys.Date()),
              textInput("loan_notes", "Notes (optional)", ""),
              actionButton("btn_record_loan", "Save Pautang", class = "btn-success w-100")
            )
          )
        ),
        column(
          6,
          card(
            card_header("Record Payment"),
            card_body(
              selectInput("payment_customer", "Customer", choices = c()),
              numericInput("payment_amount", "Amount Paid", value = 0, min = 0),
              dateInput("payment_date", "Date", value = Sys.Date()),
              textInput("payment_notes", "Notes (optional)", ""),
              actionButton("btn_record_payment", "Save Payment", class = "btn-warning w-100")
            )
          )
        )
      )
    ),
    
    # Tab 3
    tabPanel(
      "Balances & History",
      br(),
      card(card_header("Customer Balances"), card_body(DTOutput("tbl_balances"))),
      br(),
      card(card_header("Loan Summary (Loans vs Payments)"), card_body(DTOutput("tbl_loan_summary"))),
      br(),
      card(card_header("List of Products Loaned"), card_body(DTOutput("tbl_loan_products"))),
      br(),
      card(card_header("All Transactions (Latest First)"), card_body(DTOutput("tbl_tx")))
    ),
    
    # Tab 4
    tabPanel(
      "QR Codes",
      br(),
      card(
        card_header("Generate Customer QR Code"),
        card_body(
          selectInput("qr_customer", "Customer", choices = c()),
          plotOutput("qr_plot", height = 280),
          downloadButton("download_qr", "Download QR as PNG")
        )
      )
    ),
    
    # Tab 5
    tabPanel(
      "Statement of Account",
      br(),
      fluidRow(
        column(
          4,
          card(
            card_header("Generate SOA"),
            card_body(
              selectInput("soa_customer", "Customer", choices = c()),
              dateRangeInput(
                "soa_dates", "Date range",
                start = Sys.Date() - 30,
                end   = Sys.Date()
              ),
              actionButton("btn_gen_soa", "Show Statement", class = "btn-primary w-100"),
              br(), br(),
              downloadButton("download_soa", "Download SOA (PDF)")
            )
          )
        ),
        column(
          8,
          card(
            card_header(textOutput("soa_title")),
            card_body(DTOutput("tbl_soa"))
          )
        )
      )
    )
  )
)

# ------------------------------------
# 3. Helper: compute SOA from DB
# ------------------------------------
compute_soa <- function(cust_id, start_date, end_date) {
  # CHANGED: IFNULL -> COALESCE, ? -> $1, $2
  bal_before_df <- dbGetQuery(
    con,
    "SELECT COALESCE(SUM(
       CASE
         WHEN tx_type = 'LOAN'    THEN amount
         WHEN tx_type = 'PAYMENT' THEN -amount
         ELSE 0
       END
     ), 0) AS bal
     FROM credit_tx
     WHERE customer_id = $1 AND tx_date < $2;",
    params = list(cust_id, start_date)
  )
  raw_before <- ifelse(nrow(bal_before_df) == 0 || is.na(bal_before_df$bal[1]),
                       0, bal_before_df$bal[1])
  bal_before <- max(raw_before, 0)
  
  # CHANGED: ? -> $1, $2, $3
  tx <- dbGetQuery(
    con,
    "SELECT
       t.tx_date,
       t.tx_type,
       p.name AS product,
       t.qty,
       t.unit_price AS unit_price,
       t.amount,
       t.notes
     FROM credit_tx t
     LEFT JOIN products p ON t.product_id = p.product_id
     WHERE t.customer_id = $1
       AND t.tx_date BETWEEN $2 AND $3
     ORDER BY t.tx_date, t.tx_id;",
    params = list(cust_id, start_date, end_date)
  )
  
  if (nrow(tx) == 0) return(data.frame())
  
  effect  <- ifelse(tx$tx_type == "LOAN", tx$amount, -tx$amount)
  running <- bal_before + cumsum(effect)
  
  tx$balance_after <- pmax(running, 0)
  tx$tx_date <- as.Date(tx$tx_date)
  colnames(tx) <- c("Date", "Type", "Product", "Qty", "UnitPrice", "Amount", "Notes", "BalanceAfter")
  tx
}

# ------------------------------------
# 4. Server
# ------------------------------------
server <- function(input, output, session) {
  
  refresh <- reactiveVal(0)
  bump_refresh <- function() refresh(refresh() + 1)
  
  customers <- reactive({
    refresh()
    dbGetQuery(con, "
      SELECT customer_id, name, nickname, contact_no, address, max_credit
      FROM customers
      WHERE is_active = 1
      ORDER BY name;
    ")
  })
  
  products <- reactive({
    refresh()
    dbGetQuery(con, "
      SELECT product_id, name, category, unit_price, stock_qty, allow_credit
      FROM products
      WHERE is_active = 1
      ORDER BY name;
    ")
  })
  
  balances <- reactive({
    refresh()
    # CHANGED: IFNULL -> COALESCE
    dbGetQuery(con, "
      SELECT
        c.customer_id,
        c.name,
        COALESCE(SUM(
          CASE
            WHEN t.tx_type = 'LOAN'    THEN t.amount
            WHEN t.tx_type = 'PAYMENT' THEN -t.amount
            ELSE 0
          END
        ), 0) AS balance
      FROM customers c
      LEFT JOIN credit_tx t ON c.customer_id = t.customer_id
      GROUP BY c.customer_id, c.name
      ORDER BY c.name;
    ")
  })
  
  loan_summary <- reactive({
    refresh()
    # CHANGED: IFNULL -> COALESCE
    df <- dbGetQuery(con, "
      SELECT
        c.customer_id,
        c.name AS customer,
        COALESCE(SUM(CASE WHEN t.tx_type = 'LOAN'    THEN t.amount ELSE 0 END), 0) AS total_loaned,
        COALESCE(SUM(CASE WHEN t.tx_type = 'PAYMENT' THEN t.amount ELSE 0 END), 0) AS total_paid,
        COALESCE(SUM(
          CASE
            WHEN t.tx_type = 'LOAN'    THEN t.amount
            WHEN t.tx_type = 'PAYMENT' THEN -t.amount
            ELSE 0
          END
        ), 0) AS balance,
        MAX(CASE WHEN t.tx_type = 'LOAN'    THEN t.tx_date END) AS last_loan_date,
        MAX(CASE WHEN t.tx_type = 'PAYMENT' THEN t.tx_date END) AS last_payment_date
      FROM customers c
      LEFT JOIN credit_tx t ON c.customer_id = t.customer_id
      GROUP BY c.customer_id, c.name
      ORDER BY c.name;
    ")
    
    today <- Sys.Date()
    lp <- as.Date(df$last_payment_date)
    ll <- as.Date(df$last_loan_date)
    
    days_pay  <- ifelse(is.na(lp), NA_integer_, as.integer(today - lp))
    days_loan <- ifelse(is.na(ll), NA_integer_, as.integer(today - ll))
    
    rating <- character(nrow(df))
    for (i in seq_len(nrow(df))) {
      bal <- df$balance[i]
      dp  <- days_pay[i]
      dl  <- days_loan[i]
      
      if (is.na(bal) || bal <= 0) {
        rating[i] <- "🟢 GOOD"
      } else if (!is.na(dp)) {
        if (dp <= 15) rating[i] <- "🟢 GOOD"
        else if (dp <= 30) rating[i] <- "🟡 AVERAGE"
        else rating[i] <- "🔴 RISKY"
      } else {
        if (!is.na(dl) && dl > 30) rating[i] <- "🔴 RISKY"
        else rating[i] <- "🟡 AVERAGE"
      }
    }
    
    df$credit_rating <- rating
    df
  })
  
  loan_products <- reactive({
    refresh()
    # CHANGED: IFNULL -> COALESCE
    dbGetQuery(con, "
      SELECT
        c.customer_id,
        c.name AS customer,
        p.name AS product,
        COALESCE(SUM(t.qty), 0)    AS total_qty_loaned,
        COALESCE(SUM(t.amount), 0) AS total_amount_loaned
      FROM credit_tx t
      JOIN customers c ON t.customer_id = c.customer_id
      JOIN products  p ON t.product_id = p.product_id
      WHERE t.tx_type = 'LOAN'
      GROUP BY c.customer_id, c.name, p.name
      ORDER BY c.name, p.name;
    ")
  })
  
  transactions <- reactive({
    refresh()
    dbGetQuery(con, "
      SELECT
        t.tx_id,
        t.tx_date,
        t.tx_type,
        c.name AS customer,
        p.name AS product,
        t.qty,
        t.unit_price,
        t.amount,
        t.notes
      FROM credit_tx t
      JOIN customers c ON t.customer_id = c.customer_id
      LEFT JOIN products p ON t.product_id = p.product_id
      ORDER BY t.tx_date DESC, t.tx_id DESC;
    ")
  })
  
  # Dropdowns
  observe({
    cust <- customers()
    choices_cust <- if (nrow(cust) > 0) setNames(cust$customer_id, cust$name) else c()
    updateSelectInput(session, "loan_customer",    choices = choices_cust)
    updateSelectInput(session, "payment_customer", choices = choices_cust)
    updateSelectInput(session, "qr_customer",      choices = choices_cust)
    updateSelectInput(session, "soa_customer",     choices = choices_cust)
  })
  
  observe({
    prods <- products()
    if (nrow(prods) > 0) {
      price_str <- fmt_peso(prods$unit_price)
      labels <- paste0(prods$name, " (", price_str, ")")
      choices_prod <- setNames(prods$product_id, labels)
    } else {
      choices_prod <- c()
    }
    updateSelectInput(session, "loan_product", choices = choices_prod)
  })
  
  # Add customer
  observeEvent(input$btn_add_customer, {
    tryCatch({
      if (!nzchar(input$cust_name)) {
        showNotification("Customer name is required.", type = "error")
        return()
      }
      
      new_name_norm <- normalize_text(input$cust_name)
      exist <- dbGetQuery(con, "SELECT name FROM customers WHERE is_active = 1;")
      
      if (nrow(exist) > 0) {
        exist_norm <- normalize_text(exist$name)
        
        if (new_name_norm %in% exist_norm) {
          showNotification("A customer with this name already exists.", type = "error")
          return()
        }
        
        dists <- adist(new_name_norm, exist_norm)
        min_d <- min(dists)
        if (!is.infinite(min_d) && min_d <= 2) {
          close_name <- exist$name[which.min(dists)]
          showNotification(
            paste0("Name looks similar to existing '", close_name, "'. Check spelling."),
            type = "error"
          )
          return()
        }
      }
      
      # CHANGED: ? -> $1...$5
      dbExecute(
        con,
        "INSERT INTO customers (name, nickname, contact_no, address, max_credit)
         VALUES ($1, $2, $3, $4, $5);",
        params = list(
          input$cust_name,
          input$cust_nick,
          input$cust_contact,
          input$cust_address,
          input$cust_max_credit
        )
      )
      showNotification("Customer added.", type = "message")
      bump_refresh()
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      message(e)
    })
  })
  
  # Add / update product
  observeEvent(input$btn_add_product, {
    tryCatch({
      if (!(nzchar(input$prod_name) && !is.na(input$prod_price) && input$prod_price >= 0)) {
        showNotification("Product name and non-negative price are required.", type = "error")
        return()
      }
      
      allow_credit_val <- ifelse(isTRUE(input$prod_allow_credit), 1, 0)
      new_prod_name    <- input$prod_name
      new_name_norm    <- normalize_text(new_prod_name)
      
      # CHANGED: ? -> $1
      existing_exact <- dbGetQuery(
        con,
        "SELECT product_id, name FROM products WHERE name = $1;",
        params = list(new_prod_name)
      )
      
      # CHANGED: ? -> $1
      existing_other <- dbGetQuery(
        con,
        "SELECT name FROM products WHERE is_active = 1 AND name <> $1;",
        params = list(new_prod_name)
      )
      
      if (nrow(existing_other) > 0) {
        exist_norm <- normalize_text(existing_other$name)
        dists      <- adist(new_name_norm, exist_norm)
        min_d      <- min(dists)
        if (!is.infinite(min_d) && min_d <= 2) {
          close_name <- existing_other$name[which.min(dists)]
          showNotification(
            paste0("Product name looks similar to existing '", close_name, "'."),
            type = "error"
          )
          return()
        }
      }
      
      if (nrow(existing_exact) > 0) {
        pid <- existing_exact$product_id[1]
        # CHANGED: ? -> $1...$5
        dbExecute(
          con,
          "UPDATE products
           SET category = $1, unit_price = $2, stock_qty = stock_qty + $3, allow_credit = $4
           WHERE product_id = $5;",
          params = list(input$prod_cat, input$prod_price, input$prod_stock, allow_credit_val, pid)
        )
        showNotification("Product updated.", type = "message")
        bump_refresh()
        return()
      }
      
      # CHANGED: ? -> $1...$5
      dbExecute(
        con,
        "INSERT INTO products (name, category, unit_price, stock_qty, allow_credit)
         VALUES ($1, $2, $3, $4, $5);",
        params = list(new_prod_name, input$prod_cat, input$prod_price, input$prod_stock, allow_credit_val)
      )
      showNotification("Product added.", type = "message")
      bump_refresh()
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      message(e)
    })
  })
  
  # Balance helper
  get_customer_balance <- function(cust_id) {
    # CHANGED: IFNULL -> COALESCE, ? -> $1
    bal_df <- dbGetQuery(
      con,
      "SELECT COALESCE(SUM(
          CASE
            WHEN tx_type = 'LOAN'    THEN amount
            WHEN tx_type = 'PAYMENT' THEN -amount
            ELSE 0
          END
        ), 0) AS bal
        FROM credit_tx
        WHERE customer_id = $1;",
      params = list(cust_id)
    )
    raw_bal <- ifelse(nrow(bal_df) == 0 || is.na(bal_df$bal[1]), 0, bal_df$bal[1])
    max(raw_bal, 0)
  }
  
  # Record LOAN
  observeEvent(input$btn_record_loan, {
    tryCatch({
      if (is.null(input$loan_customer) || is.null(input$loan_product)) {
        showNotification("Please select a customer and product.", type = "error")
        return()
      }
      if (input$loan_qty <= 0) {
        showNotification("Quantity must be greater than zero.", type = "error")
        return()
      }
      
      cust_id <- as.integer(input$loan_customer)
      prod_id <- as.integer(input$loan_product)
      
      # CHANGED: ? -> $1
      prod_df <- dbGetQuery(
        con,
        "SELECT unit_price, allow_credit, stock_qty FROM products WHERE product_id = $1;",
        params = list(prod_id)
      )
      if (nrow(prod_df) == 0) {
        showNotification("Product not found.", type = "error")
        return()
      }
      
      unit_price   <- prod_df$unit_price[1]
      allow_credit <- prod_df$allow_credit[1]
      stock_qty    <- prod_df$stock_qty[1]
      
      if (!is.na(allow_credit) && allow_credit == 0) {
        showNotification("This product is not allowed for pautang.", type = "error")
        return()
      }
      if (!is.na(stock_qty) && stock_qty < input$loan_qty) {
        showNotification(
          paste0("Not enough stock. Available: ", stock_qty, ", requested: ", input$loan_qty),
          type = "error"
        )
        return()
      }
      
      amount <- unit_price * input$loan_qty
      
      # CHANGED: ? -> $1
      cust_info <- dbGetQuery(
        con,
        "SELECT max_credit FROM customers WHERE customer_id = $1;",
        params = list(cust_id)
      )
      max_credit <- ifelse(nrow(cust_info) == 0 || is.na(cust_info$max_credit[1]),
                           0, cust_info$max_credit[1])
      
      current_balance <- get_customer_balance(cust_id)
      if (max_credit > 0 && current_balance + amount > max_credit) {
        showNotification("Loan exceeds customer's max credit limit.", type = "error")
        return()
      }
      
      # CHANGED: ? -> $1...$8
      dbExecute(
        con,
        "INSERT INTO credit_tx
         (customer_id, tx_date, tx_type, product_id, qty, unit_price, amount, notes)
         VALUES ($1, $2, 'LOAN', $3, $4, $5, $6, $7);",
        params = list(
          cust_id,
          as.character(input$loan_date),
          prod_id,
          input$loan_qty,
          unit_price,
          amount,
          input$loan_notes
        )
      )
      
      # CHANGED: ? -> $1, $2
      dbExecute(
        con,
        "UPDATE products
         SET stock_qty = stock_qty - $1
         WHERE product_id = $2;",
        params = list(input$loan_qty, prod_id)
      )
      
      showNotification("Pautang recorded.", type = "message")
      bump_refresh()
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      message(e)
    })
  })
  
  # Record PAYMENT
  observeEvent(input$btn_record_payment, {
    tryCatch({
      if (is.null(input$payment_customer)) {
        showNotification("Please select a customer.", type = "error")
        return()
      }
      if (input$payment_amount <= 0) {
        showNotification("Payment amount must be greater than zero.", type = "error")
        return()
      }
      
      cust_id <- as.integer(input$payment_customer)
      current_balance <- get_customer_balance(cust_id)
      
      if (current_balance <= 0) {
        showNotification("Customer has no outstanding balance.", type = "error")
        return()
      }
      
      if (input$payment_amount > current_balance) {
        showNotification(
          paste0("Payment is larger than current balance (", fmt_peso(current_balance), ")."),
          type = "error"
        )
        return()
      }
      
      # CHANGED: ? -> $1...$4
      dbExecute(
        con,
        "INSERT INTO credit_tx (customer_id, tx_date, tx_type, amount, notes)
         VALUES ($1, $2, 'PAYMENT', $3, $4);",
        params = list(
          cust_id,
          as.character(input$payment_date),
          input$payment_amount,
          input$payment_notes
        )
      )
      
      showNotification("Payment recorded.", type = "message")
      bump_refresh()
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      message(e)
    })
  })
  
  # QR
  output$qr_plot <- renderPlot({
    req(input$qr_customer)
    value <- paste0("CUST:", input$qr_customer)
    
    qr <- qrcode::qr_code(value)
    par(mar = c(0, 0, 0, 0))
    plot(qr)
  })
  
  output$download_qr <- downloadHandler(
    filename = function() paste0("customer_", input$qr_customer, "_qr.png"),
    contentType = "image/png",
    content = function(file) {
      req(input$qr_customer)
      value <- paste0("CUST:", input$qr_customer)
      
      qr <- qrcode::qr_code(value)
      
      png(file, width = 900, height = 900, res = 150)
      par(mar = c(0, 0, 0, 0))
      plot(qr)
      dev.off()
    }
  )
  
  # SOA
  soa_data <- eventReactive(input$btn_gen_soa, {
    req(input$soa_customer, input$soa_dates)
    cust_id    <- as.integer(input$soa_customer)
    start_date <- as.character(input$soa_dates[1])
    end_date   <- as.character(input$soa_dates[2])
    compute_soa(cust_id, start_date, end_date)
  })
  
  output$soa_title <- renderText({
    req(input$soa_customer, input$soa_dates)
    cust <- customers()
    nm <- cust$name[cust$customer_id == as.integer(input$soa_customer)]
    if (length(nm) == 0) nm <- "Unknown"
    paste0("Statement of Account - ", nm,
           " (", as.character(input$soa_dates[1]), " to ",
           as.character(input$soa_dates[2]), ")")
  })
  
  output$tbl_soa <- renderDT({
    df <- soa_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No transactions in selected period."),
                       options = list(dom = "t")))
    }
    view_df <- df
    view_df$UnitPrice    <- fmt_peso(view_df$UnitPrice)
    view_df$Amount       <- fmt_peso(view_df$Amount)
    view_df$BalanceAfter <- fmt_peso(view_df$BalanceAfter)
    datatable(view_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # PDF SOA
  output$download_soa <- downloadHandler(
    filename = function() {
      cust <- customers()
      nm <- cust$name[cust$customer_id == as.integer(input$soa_customer)]
      if (length(nm) == 0) nm <- "customer"
      paste0("SOA_", gsub(" ", "_", nm), "_",
             as.character(input$soa_dates[1]), "_to_", as.character(input$soa_dates[2]), ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      req(input$soa_customer, input$soa_dates)
      cust_id    <- as.integer(input$soa_customer)
      start_date <- as.character(input$soa_dates[1])
      end_date   <- as.character(input$soa_dates[2])
      df         <- compute_soa(cust_id, start_date, end_date)
      
      cust <- customers()
      nm <- cust$name[cust$customer_id == cust_id]
      if (length(nm) == 0) nm <- "customer"
      
      if (is.null(df) || nrow(df) == 0) {
        pdf_df <- data.frame(Message = "No transactions in selected period.")
      } else {
        pdf_df <- df
        pdf_df$UnitPrice    <- fmt_peso(pdf_df$UnitPrice)
        pdf_df$Amount       <- fmt_peso(pdf_df$Amount)
        pdf_df$BalanceAfter <- fmt_peso(pdf_df$BalanceAfter)
      }
      
      if (capabilities("cairo")) {
        cairo_pdf(file, width = 8.27, height = 11.69)
      } else {
        pdf(file, width = 8.27, height = 11.69)
      }
      
      par(mai = c(0.5, 0.5, 0.5, 0.5))
      plot.new()
      text(0.5, 0.95, paste0("Statement of Account - ", nm), cex = 1.4, font = 2)
      text(0.5, 0.90, paste0("Period: ", start_date, " to ", end_date), cex = 1.0)
      
      g <- gridExtra::tableGrob(pdf_df, rows = NULL)
      grid::pushViewport(grid::viewport(
        x = 0.5, y = 0.45, width = 0.95, height = 0.7,
        just = c("center", "center")
      ))
      grid::grid.draw(g)
      dev.off()
    }
  )
  
  # Tables (DT)
  output$tbl_customers <- renderDT({
    datatable(customers(), options = list(pageLength = 8, scrollX = TRUE))
  })
  
  output$tbl_products <- renderDT({
    df <- products()
    df$allow_credit <- ifelse(df$allow_credit == 1, "Yes", "No")
    df$stock_qty    <- pmax(df$stock_qty, 0L)
    df$unit_price   <- fmt_peso(df$unit_price)
    colnames(df) <- c("ID", "Name", "Category", "UnitPrice", "StockQty", "AllowedPautang")
    datatable(df, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  output$tbl_balances <- renderDT({
    df <- balances()
    ls <- loan_summary()
    df$credit_rating <- ls$credit_rating[match(df$customer_id, ls$customer_id)]
    df$balance <- fmt_peso(pmax(df$balance, 0))
    colnames(df) <- c("CustomerID", "Name", "Balance", "CreditRating")
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_loan_summary <- renderDT({
    df <- loan_summary()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No loans recorded yet."), options = list(dom = "t")))
    }
    view_df <- df
    view_df$total_loaned <- fmt_peso(view_df$total_loaned)
    view_df$total_paid    <- fmt_peso(view_df$total_paid)
    view_df$balance       <- fmt_peso(pmax(view_df$balance, 0))
    view_df <- view_df[, c("customer_id", "customer", "credit_rating", "total_loaned", "total_paid", "balance")]
    colnames(view_df) <- c("CustomerID", "Customer", "CreditRating", "TotalLoaned", "TotalPaid", "OutstandingBalance")
    # Corrected the parenthesis here too
    datatable(view_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_loan_products <- renderDT({
    df <- loan_products()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No product loans recorded yet."), options = list(dom = "t")))
    }
    df$total_amount_loaned <- fmt_peso(df$total_amount_loaned)
    colnames(df) <- c("CustomerID", "Customer", "Product", "TotalQtyLoaned", "TotalAmountLoaned")
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_tx <- renderDT({
    df <- transactions()
    df$unit_price <- fmt_peso(df$unit_price)
    df$amount     <- fmt_peso(df$amount)
    colnames(df) <- c("TxID", "Date", "Type", "Customer", "Product", "Qty", "UnitPrice", "Amount", "Notes")
    datatable(df, options = list(pageLength = 12, scrollX = TRUE, order = list(list(1, "desc"))))
  })
}

shinyApp(ui = ui, server = server)
