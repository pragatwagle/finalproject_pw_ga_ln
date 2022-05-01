#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RMariaDB)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyauthr)
library(plotly) 
library(shinythemes) 


# Define UI for application that draws a histogram
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

ui <- fluidPage(theme =shinytheme("sandstone"),
                
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  # Sidebar to show user info after login
  uiOutput("appPage")
  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  observe({
    if(credentials()$user_auth) {
      shinyjs::addClass(selector = "Application", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  output$sidebarpanel <- renderUI({
    
    # Show only when authenticated
    req(credentials()$user_auth)
    
    tagList(
      # Sidebar with a slider input
      column(width = 4,
             sliderInput("obs",
                         "Number of observations:",
                         min = 0,
                         max = 1000,
                         value = 500)
      ),
      
      column(width = 4,
             p(paste("You have", credentials()$info[["permissions"]],"permission"))
      )
    )
    
  })
  
  
  output$appPage <- renderUI({
    
    # Show only when authenticated
    req(credentials()$user_auth)
    
    tabBox(width = 12,
           tabPanel(title = " Category",
                    tabBox(width = 12,
                           tabPanel(title = "Select Category",
                                    
                                    wellPanel(
                                      br(),
                                      actionButton("Select_Category", "Select"),
                                      br(),
                                      br()),
                                    fluidRow(
                                      column(12,
                                             dataTableOutput('CategoryTable')
                                      )
                                    )
                                    
                           ),                   
                           tabPanel(title = "Add Category",
                                    
                                    wellPanel(
                                      fluidRow(column(width = 12,"Add Category")),
                                      br(),
                                      fluidRow(column(width = 12,textInput("category_id_addcategory", "Enter ID"))),
                                      br(),
                                      fluidRow(column(width = 12,textInput("category_code_addcategory", "Enter Category"))),
                                      actionButton("Insert_Category", "Add"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("AddCategoryReturnText"))
                           ),
                           tabPanel(title = "Update Category",
                                    wellPanel(
                                      fluidRow(column(width = 12,"Update Category")),
                                      br(),
                                      fluidRow(column(width = 12,textInput("category_id_updatecategory", "Enter Category Code to Update"))),
                                      fluidRow(column(width = 12,textInput("category_code_updatecategory", "Enter New Category"))),
                                      actionButton("Update_Category", "Update"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("UpdateCategoryReturnText"))
                           ),
                           tabPanel(title = "Delete Category",
                                    wellPanel(
                                      fluidRow(column(width = 12,"Delete Category")),
                                      br(),
                                      fluidRow(column(width = 12,textInput("category_code_deletecategory", "Enter Category"))),
                                      actionButton("Delete_Category", "Delete Category"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("DeleteCategoryReturnText"))
                           ),
                           tabPanel(title="Visualize Category",
                                  wellPanel(
                                    actionButton("View_Category", "View Top Category"),
                                    br(),
                                    plotOutput("plotCat")
                                  )  
                            )
                    )
           ),
           tabPanel(title = " Product",
                    tabBox(width = 12,
                           tabPanel(title = "Select Products",
                                    wellPanel(
                                      br(),
                                      actionButton("Select_Products", "Select"),
                                      br(),
                                      br()),
                                    fluidRow(
                                      column(12,
                                             dataTableOutput('ProductsTable')
                                      )
                                    )
                                    
                           ), 
                           tabPanel(title = "Add Product",
                                    wellPanel(
                                      fluidRow(column(width = 12)),
                                      br(),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("id_addproduct", "Product Id"
                                               ))
                                      ),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               textInput("brand_addproduct", "Enter Brand"
                                               ))
                                      ),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               textInput("category_id_addproduct", "Enter Category ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("price_addproduct", "Enter Price"
                                               ))
                                      ),
                                      br(),
                                      actionButton("Insert_Product", "Add Product"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("AddProductReturnText")
                                      
                                    )
                                    
                           ),
                           tabPanel(title = "Update Product",
                                    wellPanel(
                                      fluidRow(column(width = 12)),
                                      br(),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("product_id_updateproduct", "Enter Product ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("brand_updateproduct", "Enter Brand"
                                               ))
                                      ),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               textInput("category_id_updateproduct", "Enter Category ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("price_updateproduct", "Enter Price"
                                               ))
                                      ),
                                      br(),
                                      actionButton("Update_Product", "Update Product"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("UpdateProductReturnText")
                                      
                                    )
                                    
                           ),
                           tabPanel(title = "Delete Product",
                                    wellPanel(
                                      fluidRow(column(width = 12,"Delete Product")),
                                      br(),
                                      fluidRow(column(width = 12,textInput("product_id_deleteproduct", "Enter Product ID"))),
                                      actionButton("Delete_Product", "Delete Product"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("DeleteProductReturnText"))
                           ),
                           tabPanel(title="Visualize Inventory",
                                    wellPanel(
                                      actionButton("View_Inventory", "Top Products in Inventory"),
                                      br(),
                                      plotOutput("plotInv")
                                    )  
                           )
                    )
           ),
           tabPanel(title = "Inventory",
                    tabBox(width = 12,
                           tabPanel(title = "Select Inventory",
                                    
                                    wellPanel(
                                      br(),
                                      actionButton("Select_Inventory", "Select"),
                                      br(),
                                      br()),
                                    fluidRow(
                                      column(12,
                                             dataTableOutput('InventoryTable')
                                      )
                                    )
                                    
                           ),    
                           tabPanel(title = "Add Inventory",
                                    
                                    wellPanel(
                                      fluidRow(column(width = 12)),
                                      br(),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("product_id_addinventory", "Enter Prouduct ID"
                                               ))
                                      ),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               textInput("added_quatity_addinventory", "Enter Amount Added"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("sold_quantity_addinventory", "Enter Amount Sold"
                                               ))
                                      ),
                                      br(),
                                      actionButton("Insert_Inventory", "Add Product"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("InsertInventoryReturnText")
                                      
                                    )
                                    
                           ),
                           tabPanel(title = "Update Inventory",
                                    wellPanel(
                                      fluidRow(column(width = 12)),
                                      br(),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("transaction_id_updateinventory", "Enter Transaction ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("product_id_updateinventory", "Enter Prouduct ID"
                                               ))
                                      ),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               textInput("added_quatity_updateinventory", "Enter Amount Added"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("sold_quantity_updateinventory", "Enter Amount Sold"
                                               ))
                                      ),
                                      br(),
                                      actionButton("Update_Inventory", "Update Inventory"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("UpdateInventoryReturnText")
                                      
                                    )
                                    
                           ),
                           
                           tabPanel(title = "Delete Inventory",
                                    wellPanel(
                                      fluidRow(column(width = 12,"Delete Inventory")),
                                      br(),
                                      fluidRow(column(width = 12,textInput("transaction_id_deleteinventory", "Enter Transaction ID"))),
                                      actionButton("Delete_Inventory", "Delete Inventory"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("DeleteInventoryReturnText"))
                           ),
                           tabPanel(title="Visualize Products",
                                    wellPanel(
                                      actionButton("View_Products", "Top Brands Available"),
                                      br(),
                                      plotOutput("plotProd")
                                    )  
                           )
                    )
           ),
           tabPanel(title = " Purchases",
                    tabBox(width = 12,
                           tabPanel(title = "Select Purchases",
                                    
                                    wellPanel(
                                      br(),
                                      actionButton("Select_Purchases", "Select"),
                                      br(),
                                      br()),
                                    fluidRow(
                                      column(12,
                                             dataTableOutput('PurchasesTable')
                                      )
                                    )
                                    
                           ),       
                           tabPanel(title = "Add Purchase",
                                    wellPanel(
                                      fluidRow(column(width = 12)),
                                      br(),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("product_id_addpurchase", "Enter Product ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("order_id_addpurchase", "Enter Order ID"
                                               ))
                                      ),
                                      
                                      fluidRow(
                                        column(width = 6,
                                               textInput("user_id_addpurchase", "Enter User ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("quantity_addpurchase", "Enter Quantity"
                                               ))
                                      ),
                                      br(),
                                      actionButton("Insert_Purchase", "Add Purchase"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("InsertPurchaseReturnText")
                                      
                                    )
                                    
                           ),
                           
                           
                           
                           tabPanel(title = "Update Purchase",
                                    wellPanel(
                                      fluidRow(column(width = 12)),
                                      br(),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("purchase_id_updatepurchase", "Purchase ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("order_id_updatepurchase", "Enter Order ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("product_id_updatepurchase", "Enter Product ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("user_id_updatepurchase", "Enter User ID"
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               textInput("quantity_updatepurchase", "Enter Quantity"
                                               ))
                                      ),
                                      br(),
                                      actionButton("Update_Purchase", "Update Purchase"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("UpdatePurchaseReturnText")
                                      
                                    )
                                    
                           ),
                           tabPanel(title = "Delete Purchase",
                                    wellPanel(
                                      fluidRow(column(width = 12,"Delete Purchase")),
                                      br(),
                                      fluidRow(column(width = 12,textInput("purchase_id_deletepurchase", "Enter Purchase ID"))),
                                      actionButton("Delete_Purchase", "Delete Purchase"),
                                      br(),
                                      br(),
                                      verbatimTextOutput("DeletePurchaseReturnText"))
                           ),
                           tabPanel(title="Visualize Purchases",
                                    wellPanel(
                                      actionButton("View_Purchases", "Top Purchased Products"),
                                      br(),
                                      plotOutput("plotPur")
                                    )  
                           )
                    )
           )
           
           
    )
    
    
  })
  
  observeEvent(input$View_Category, {
    req(credentials()$user_auth)
    selectCategoryQuery<-paste(
      "select category_code,  count(category_code) as total from Products p 
        join Category c 
        on c.category_id = p.category_id
        group by c.category_code
        order by total desc
        limit 8"
    )
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    
      res <- dbGetQuery(storeDB, selectCategoryQuery)
      # converting table into dataframes, easy to plot

      # Barplot using plotly
        output$plotCat <- renderPlot({
          slices <- res$total
          lbls <- res$category_code
          pct <- round(slices/sum(slices)*100)
          lbls <- paste(lbls, pct) # add percents to labels
          lbls <- paste(lbls,"%",sep="") # ad % to labels
          pie(slices,labels = lbls, col=rainbow(length(lbls)),
              main="Top Categories Available ")
          
        })

        
   
    
    dbDisconnect(storeDB)
  })
  observeEvent(input$View_Inventory, {
    req(credentials()$user_auth)
    selectCategoryQuery<-paste(
      "SELECT product_id, sum(added_quantity - sold_quantity) as Quantity 
      FROM Inventory
      group by product_id
      order by Quantity desc
      Limit 6"
    )
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    
    res <- dbGetQuery(storeDB, selectCategoryQuery)
    # converting table into dataframes, easy to plot
    
    # Barplot using plotly
    output$plotInv <- renderPlot({
      slices <- res$Quantity
      lbls <- res$product_id
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=rainbow(length(lbls)),
          main="Highest Quantity Products in Inventory")
      
    })
    
    
    
    
    dbDisconnect(storeDB)
  })
  observeEvent(input$View_Products, {
    req(credentials()$user_auth)
    selectCategoryQuery<-paste(
      "SELECT brand as Brand, count(brand) as TotalCount 
      FROM Products
      GROUP BY Brand
      ORDER by TotalCount desc
      Limit 5;"
    )
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    
    res <- dbGetQuery(storeDB, selectCategoryQuery)
    # converting table into dataframes, easy to plot
    
    # Barplot using plotly
    output$plotProd <- renderPlot({
      slices <- res$TotalCount
      lbls <- res$Brand
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=rainbow(length(lbls)),
          main="Count of Top Brands Available ")
      
    })
    
    
    
    
    dbDisconnect(storeDB)
  })
  observeEvent(input$View_Purchases, {
    req(credentials()$user_auth)
    selectCategoryQuery<-paste(
      "SELECT product_id as TopProducts , count(product_id) as TimesSold 
        FROM PurchasedProducts
        group by product_id
        order by TimesSold desc
        limit 5"
    )
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    
    res <- dbGetQuery(storeDB, selectCategoryQuery)
    # converting table into dataframes, easy to plot
    
    # Barplot using plotly
    output$plotPur <- renderPlot({
      slices <- res$TimesSold
      lbls <- res$TopProducts
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=rainbow(length(lbls)),
          main="Top Purchased Products ")
      
    })
    
    
    
    
    dbDisconnect(storeDB)
  })
  
  
  observeEvent(input$Select_Category, {
    req(credentials()$user_auth)
    
    category_code_addcategory <- input$category_code_addcategory
    selectCategoryQuery<-paste(
      "SELECT * FROM Category"
    )
    output$CategoryTable <- renderDataTable({
      #storeDB <- dbConnect(RMariaDB::MariaDB(), user='id18866633_d591', password='Pragat1012!!', dbname='id18866633_inventory', host='127.0.0.1')
      storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
      
      res <- dbGetQuery(storeDB, selectCategoryQuery)

      dbDisconnect(storeDB)
      res      
    })

    
  })
  
  observeEvent(input$Select_Products, {
    req(credentials()$user_auth)
    
    
    selectProductsQuery<-paste(
      "SELECT Distinct product_id, brand FROM Products"
    )
    output$ProductsTable <- renderDataTable({
      storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
      res <- dbGetQuery(storeDB, selectProductsQuery)
      
      dbDisconnect(storeDB)
      res      
    })
    
    
  })
  
  observeEvent(input$Select_Inventory, {
    req(credentials()$user_auth)
    selectInventoryQuery<-paste(
      "SELECT * FROM Inventory"
    )
    output$InventoryTable <- renderDataTable({
      storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
      res <- dbGetQuery(storeDB, selectInventoryQuery)
      
      dbDisconnect(storeDB)
      res      
    })
    
    
  })
  observeEvent(input$Select_Purchases, {
    req(credentials()$user_auth)
    selectPurchasesQuery<-paste(
      "SELECT * FROM PurchasedProducts"
    )
    output$PurchasesTable <- renderDataTable({
      storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
      res <- dbGetQuery(storeDB, selectPurchasesQuery)
      
      dbDisconnect(storeDB)
      res      
    })
    
    
  }) 
  
  
  observeEvent(input$Insert_Category, {
    req(credentials()$user_auth)
    category_code_addcategory <- input$category_code_addcategory
    category_id_addcategory <- input$category_id_addcategory
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    insertCategoryQuery<-paste(
      "INSERT INTO Category (category_id, category_code)
              VALUES(TRIM('",category_id_addcategory,"'),TRIM('",category_code_addcategory,"'))"
    )
    
    rsInsert <- dbSendQuery(storeDB, insertCategoryQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$AddCategoryReturnText <- renderText({paste(category_code_addcategory, "has been added")})
    }else{
      output$AddCategoryReturnText <- renderText({paste(category_code_addcategory, "has not been added")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  
  observeEvent(input$Insert_Product, {
    req(credentials()$user_auth)
    id_addproduct <- input$id_addproduct
    brand_addproduct <- input$brand_addproduct
    category_id_addproduct <- input$category_id_addproduct
    price_addproduct <- input$price_addproduct
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    insertProductQuery<-paste(
      "INSERT INTO Products (product_id, brand,category_id,price)
              VALUES(TRIM('",id_addproduct,"'), TRIM('",brand_addproduct,"'),TRIM('",category_id_addproduct,"'),TRIM('",price_addproduct,"'))"
    )
    
    
    rsInsert <- dbSendQuery(storeDB, insertProductQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$AddProductReturnText <- renderText({paste( "Product has been added.")})
    }else{
      output$AddProductReturnText <- renderText({paste("Product has not been added")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  observeEvent(input$Insert_Inventory, {
    req(credentials()$user_auth)
    
    product_id_addinventory <- input$product_id_addinventory
    added_quatity_addinventory <- input$added_quatity_addinventory
    sold_quantity_addinventory <- input$sold_quantity_addinventory
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    insertInventoryQuery<-paste(
      "INSERT INTO Inventory (product_id,added_quantity,sold_quantity)
              VALUES(TRIM('",product_id_addinventory,"'),TRIM('",added_quatity_addinventory,"'),TRIM('",sold_quantity_addinventory,"'))"
    )
    
    rsInsert <- dbSendQuery(storeDB, insertInventoryQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$InsertInventoryReturnText <- renderText({paste( "Inventory has been added.")})
    }else{
      output$InsertInventoryReturnText <- renderText({paste("Inventory has not been added")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  observeEvent(input$Insert_Purchase, {
    req(credentials()$user_auth)
    product_id_addpurchase <- input$product_id_addpurchase
    order_id_addpurchase <- input$order_id_addpurchase
    user_id_addpurchase <- input$user_id_addpurchase
    quantity_addpurchase <- input$quantity_addpurchase
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    insertpurchaseuery<-paste(
      "INSERT INTO PurchasedProducts (product_id, order_id,user_id,quantity)
              VALUES(TRIM('",product_id_addpurchase,"'),TRIM('",order_id_addpurchase,"'),TRIM('",user_id_addpurchase,"'),TRIM('",quantity_addpurchase,"'))"
    )
    
    rsInsert <- dbSendQuery(storeDB, insertpurchaseuery)
    if(dbHasCompleted(rsInsert))
    {
      output$InsertPurchaseReturnText <- renderText({paste( "Purchase has been added.")})
    }else{
      output$InsertPurchaseReturnText <- renderText({paste("Purchase has not been added")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  observeEvent(input$Update_Category, {
    req(credentials()$user_auth)
    category_id_updatecategory <- input$category_id_updatecategory
    category_code_updatecategory <- input$category_code_updatecategory
    
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    updateCategoryQuery<-paste(
      "UPDATE Category SET category_code = TRIM('",category_code_updatecategory,"')  WHERE category_code = TRIM('",category_id_updatecategory,"')"
    )
    rsInsert <- dbSendQuery(storeDB, updateCategoryQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$UpdateCategoryReturnText <- renderText({paste( "Category has been update")})
    }else{
      output$UpdateCategoryReturnText <- renderText({paste("Category has not been update")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  observeEvent(input$Update_Product, {
    req(credentials()$user_auth)
    product_id_updateproduct <- input$product_id_updateproduct
    brand_updateproduct <- input$brand_updateproduct
    category_id_updateproduct <- input$category_id_updateproduct
    price_updateproduct <- input$price_updateproduct
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    updateProductquery<-paste(
      "UPDATE Products SET brand = TRIM('",brand_updateproduct,"'),category_id = TRIM('",category_id_updateproduct,"'),price = TRIM('",price_updateproduct,"')   WHERE product_id = TRIM('",product_id_updateproduct,"')"
    )
    
    rsInsert <- dbSendQuery(storeDB, updateProductquery)
    if(dbHasCompleted(rsInsert))
    {
      output$UpdateProductReturnText <- renderText({paste( "Product has been updated")})
    }else{
      output$UpdateProductReturnText <- renderText({paste("Product has not been updated")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  
  observeEvent(input$Update_Inventory, {
    req(credentials()$user_auth)
    transaction_id_updateinventory <- input$transaction_id_updateinventory
    product_id_updateinventory <- input$product_id_updateinventory
    added_quatity_updateinventory <- input$added_quatity_updateinventory
    category_id_updateproduct <- input$category_id_updateproduct
    sold_quantity_updateinventory <- input$sold_quantity_updateinventory
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    updateProductquery<-paste(
      "UPDATE Inventory SET product_id = TRIM('",product_id_updateinventory,"'),added_quantity = TRIM('",added_quatity_updateinventory,"'),sold_quantity = TRIM('",sold_quantity_updateinventory,"')   WHERE transaction_id = TRIM('",transaction_id_updateinventory,"')"
    )
    
    rsInsert <- dbSendQuery(storeDB, updateProductquery)
    if(dbHasCompleted(rsInsert))
    {
      output$UpdateInventoryReturnText <- renderText({paste( "Inventory has been updated")})
    }else{
      output$UpdateInventoryReturnText <- renderText({paste("Inventory has not been updated")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  
  observeEvent(input$Update_Purchase, {
    req(credentials()$user_auth)
    purchase_id_updatepurchase <- input$purchase_id_updatepurchase
    order_id_updatepurchase <- input$order_id_updatepurchase
    product_id_updatepurchase <- input$product_id_updatepurchase
    user_id_updatepurchase <- input$user_id_updatepurchase
    quantity_updatepurchase <- input$quantity_updatepurchase
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    updatePurchaseQuery<-paste(
      "UPDATE PurchasedProducts SET user_id = TRIM('",user_id_updatepurchase,"'),product_id = TRIM('",product_id_updatepurchase,"'),quantity = TRIM('",quantity_updatepurchase,"'),order_id = TRIM('",order_id_updatepurchase,"') WHERE purchase_id = TRIM('",purchase_id_updatepurchase,"')"
    )
    rsInsert <- dbSendQuery(storeDB, updatePurchaseQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$UpdatePurchaseReturnText <- renderText({paste( "Purchase has been updated")})
    }else{
      output$UpdatePurchaseReturnText <- renderText({paste("Purchase has not been updated")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
  observeEvent(input$Delete_Category, {
    req(credentials()$user_auth)
    category_code_deletecategory <- input$category_code_deletecategory
    
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    deleteCategoryQuery<-paste(
      "DELETE FROM Category WHERE category_code = TRIM('",category_code_deletecategory,"')"
    )
    rsInsert <- dbSendQuery(storeDB, deleteCategoryQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$DeleteCategoryReturnText <- renderText({paste( "Category has been deleted")})
    }else{
      output$DeleteCategoryReturnText <- renderText({paste("Category has not been deleted")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  observeEvent(input$Delete_Product, {
    req(credentials()$user_auth)
    
    product_id_deleteproduct <- input$product_id_deleteproduct
    
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    deleteProductQuery<-paste(
      "DELETE FROM Products WHERE product_id = TRIM('",product_id_deleteproduct,"')"
    )
    rsInsert <- dbSendQuery(storeDB, deleteProductQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$DeleteProductReturnText <- renderText({paste( "Product has been deleted")})
    }else{
      output$DeleteProductReturnText <- renderText({paste("Product has not been deleted")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  observeEvent(input$Delete_Inventory, {
    req(credentials()$user_auth)
    
    transaction_id_deleteinventory <- input$transaction_id_deleteinventory
    
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    deleteInventoryQuery<-paste(
      "DELETE FROM Inventory WHERE transaction_id = TRIM('",transaction_id_deleteinventory,"')"
    )
    rsInsert <- dbSendQuery(storeDB, deleteInventoryQuery)
    if(dbHasCompleted(rsInsert))
    {
      output$DeleteInventoryReturnText <- renderText({paste( "Inventory has been deleted")})
    }else{
      output$DeleteInventoryReturnText <- renderText({paste("Inventory has not been deleted")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  observeEvent(input$Delete_Purchase, {
    req(credentials()$user_auth)
    
    purchase_id_deletepurchase <- input$purchase_id_deletepurchase
    
    storeDB <- dbConnect(RMariaDB::MariaDB(), user='uypj8ogzuvfazzt0', password='N63YwkMTnbtuNkk8G54L', dbname='bsgi0okg6qza9wf08hn8', host='bsgi0okg6qza9wf08hn8-mysql.services.clever-cloud.com', port ='3306')
    deletPurchaseQuery<-paste(
      "DELETE FROM PurchasedProducts WHERE purchase_id = TRIM('",purchase_id_deletepurchase,"')"
    )
    rsInsert <- dbSendQuery(storeDB, deletPurchaseQuery)
    if(dbHasCompleted(rsInsert))
    { 
      output$DeletePurchaseReturnText <- renderText({paste( "Purchase has been deleted")})
    }else{
      output$DeletePurchaseReturnText <- renderText({paste("Purchase has not been deleted")})
    }
    
    # Clear the result.
    dbClearResult(rsInsert)
    
    # Disconnect to clean up the connection to the database.
    dbDisconnect(storeDB)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
