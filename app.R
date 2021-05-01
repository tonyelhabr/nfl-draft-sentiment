library(shiny)
library(tidyverse)
library(teamcolors)
library(ggimage)
library(ggrepel)
library(magick)
library(grid)
library(ggpmisc)
library(extrafont)
library(lubridate)
library(hms)
library(shinythemes)
library(here)
library(httr)
library(googledrive)
# loadfonts(device = "win", quiet = TRUE)
# options(gargle_verbosity = "debug")
options(readr.num_columns = 0)
Sys.setenv(TZ = 'America/New_York')

### GLOBAL VARIABLES
logos <- 
  read_csv(here('data', "logos.csv")) %>% 
  select(abbrv = abbrv_nflfastr, team = team_nflfastr, team_logo, draft_franchise_id)
font <- '' # 'Roboto' # get rid of this cuz windows -> linux (shinyapps.io) is bad

# drive_auth_configure(path = here('.R', 'gargle', 'client_secret.json'), active = FALSE)
options(
  gargle_oauth_cache = '.secrets',
  gargle_oauth_email = TRUE
)

refresh_draft_order <- function(...) {
  resp <- 
    'https://www.pff.com/api/mock_draft_simulator/draft_picks' %>%
    httr::GET() %>%
    httr::content('parsed') 
  
  picks <-
    resp %>% 
    pluck('draft_picks') %>% 
    enframe() %>% 
    hoist(
      value,
      'round' = 'round',
      'pick' = 'pick',
      'draft_franchise_id' = 'draft_franchise_id',
      'player_id' = 'player_id',
      .ptype = list('player_id' = integer())
    ) %>% 
    select(-c(name, value))
  
  resp_bb <- 
    'https://www.pff.com/api/college/big_board?season=2021&version=4' %>%
    httr::GET() %>%
    httr::content('parsed')
  
  players <-
    resp_bb %>% 
    pluck('players') %>% 
    # Avoid `name` naming conflict.
    enframe('i') %>% 
    hoist(
      value,
      'player_id' = 'id',
      'player' = 'name',
      .ptype = list('player_id' = integer())
    ) %>% 
    select(-c(i, value))
  players
  
  draft_order <-
    picks %>% 
    inner_join(logos %>% select(abbrv, team, draft_franchise_id), by = 'draft_franchise_id') %>% 
    left_join(players, by = 'player_id') %>% 
    select(
      round,
      pick,
      draft_franchise_id,
      abbrv,
      team,
      player_id,
      player
    )
  write_csv(draft_order, here('data', 'draft_order.csv'))
  draft_order
}

# Do when opening the app
draft_order <- refresh_draft_order()

# Not actually using any user input here, since we should always be re-downloading from the drive.
f_read <- function(...) {
  cat(sprintf('%s: Re-reading input.', Sys.time()), sep = '\n')

  from_googldrive <- TRUE
  if(from_googldrive) {
    # googledrive::drive_auth(path = here::here('.R', 'gargle', 'client_secret.json'))
    drive_files <- googledrive::drive_find(pattern = 'comments', type = 'csv', n_max = 2) # download from your drive
    comments_file <- drive_files %>% filter(name == 'comments.csv')
    res_dl <-
      comments_file %>%
      drive_download(path = here('data', 'comments.csv'), overwrite = TRUE)
    res <- res_dl$local_path %>% read.csv(sep="\t", stringsAsFactors = FALSE, quote="")
  } else {
    res <- read.csv(here('data', 'comments.csv'), sep="\t", stringsAsFactors = FALSE, quote="") 
  }
  cat(sprintf('%s: # of rows: %s', Sys.time(), nrow(res)), sep = '\n')
  res %>% 
    mutate(team = if_else(team == 'Redskins', 'Football Team', team))
}


nfl_teams <- teamcolors %>%
  filter(league == "nfl") %>%
  dplyr::select(mascot, division, primary) %>%
  rename(team = mascot) %>%
  mutate(team = if_else(team == 'Redskins', 'Football Team', team)) %>% 
  left_join(logos) %>%
  mutate(team_logo = as.character(team_logo))
logo_grobs <- image_read(nfl_teams$team_logo)

###### CREATE GROB FOR EACH LOGO
nfl_grobs <- list()
for (i in 1:nrow(nfl_teams)) {
  nfl_grobs[[i]] <- rasterGrob(image = logo_grobs[i])
}
nfl_teams$logo_grob <- nfl_grobs

# SETUP GAUGE PLOT FUNCTION
gg.gauge <- function(pos, breaks = c(0, 33, 66, 100), determinent, team_name) {
  # get time
  t <- Sys.time()
  t_string <- strftime(t, "%I:%M %p")

  get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
    th.start <- pi * (1 - a / 100)
    th.end <- pi * (1 - b / 100)
    th <- seq(th.start, th.end, length = 500)
    x <- r1 * cos(th)
    xend <- r2 * cos(th)
    y <- r1 * sin(th)
    yend <- r2 * sin(th)
    data.frame(x, y, xend, yend)
  }

  if (team_name == "goodell") {
    cap <- paste("")
  } else {
    cap <- paste("Sentiment for", team_name, "fans at", t_string, "\n (prior 5 minutes)")
  }

  # colors from https://flatuicolors.com/palette/defo
  ggplot() +
    geom_segment(
      data = get.poly(breaks[1], breaks[4]),
      aes(x = x, y = y, xend = xend, yend = yend, color = xend),
      size = 2
    ) +
    scale_color_gradientn(colors = c("#c0392b", "#f39c12", "#27ae60")) +
    geom_segment(data = get.poly(pos - 1, pos + 1, 0.1), aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(
      data = as.data.frame(breaks),
      size = 5,
      vjust = 0,
      aes(x = 0.8 * cos(pi * (1 - breaks / 100)), y = -0.1),
      label = c("BOO", "", "", "CHEER"),
      fontface = "bold"
    ) +
    annotate("text", x = 0, y = 0, label = determinent, vjust = 0, size = 8, fontface = "bold") +
    coord_fixed() +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      text = element_text(family = font),
      plot.caption = element_text(hjust = 0.5, size = rel(1.2))
    ) +
    labs(
      title = "",
      caption = cap
    )
}

gg.gauge2 <- function(pos, breaks = c(0, 33, 66, 100), determinent, team_name) {
  # get time
  t <- Sys.time()
  t_string <- strftime(t, "%I:%M %p")

  get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
    th.start <- pi * (1 - a / 100)
    th.end <- pi * (1 - b / 100)
    th <- seq(th.start, th.end, length = 500)
    x <- r1 * cos(th)
    xend <- r2 * cos(th)
    y <- r1 * sin(th)
    yend <- r2 * sin(th)
    data.frame(x, y, xend, yend)
  }
  # colors from https://flatuicolors.com/palette/defo
  ggplot() +
    geom_segment(
      data = get.poly(breaks[1], breaks[4]),
      aes(x = x, y = y, xend = xend, yend = yend, color = xend),
      size = 2
    ) +
    scale_color_gradientn(colors = c("#27ae60", "#f39c12", "#c0392b")) +
    geom_segment(data = get.poly(pos - 1, pos + 1, 0.1), aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(
      data = as.data.frame(breaks),
      size = 5,
      vjust = 0,
      aes(x = 0.8 * cos(pi * (1 - breaks / 100)), y = -0.1),
      label = c("", "", "", "BOO"),
      fontface = "bold"
    ) +
    annotate("text", x = 0, y = 0, label = determinent, vjust = 0, size = 8, fontface = "bold") +
    coord_fixed() +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      text = element_text(family = font),
      plot.caption = element_text(hjust = 0.5, size = rel(1.2))
    ) +
    labs(
      title = "",
      caption = paste("Number of boos by all NFL fans at", t_string, "\n (prior 5 minutes)")
    )
}


### SETUP PICK ORDER


ui <- fluidPage(
  # avoid greying out plot while recalculating
  tags$style(
    type = "text/css",
    ".recalculating {opacity: 1.0;};
               .row
               .container-fluid "
  ),
  headerPanel("NFL Draft Boo Meter"),
  sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(
      width = 3,
      style = "text-overflow: clip; font-size: 18px",
      htmlOutput("recentComments")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      width = 9,
      style = "font-size: 18px",
      column(
        6,
        fluidRow(
          plotOutput("maPlot", height = "400px"),
          actionButton("pickback", "", icon = icon("arrow-left", lib = "glyphicon")),
          actionButton("picknext", "", icon = icon("arrow-right", lib = "glyphicon")),
          actionButton("pickrefresh", "", icon = icon("refresh", lib = "glyphicon"))
          # textBox("picktype", "", icon = icon("refresh", lib = "glyphicon"))
        ),
        fluidRow(column(6, htmlOutput("gaugeInfo")), column(6, plotOutput("gaugePlot", height = "250px"))),
        hr(),
        fluidRow(column(6, htmlOutput("gaugeInfo2")), column(6, plotOutput("gaugePlot2", height = "250px")))
      ),
      column(
        4,
        fluidRow(plotOutput("sentPlot", width = "400px", height = "475px"), style = "padding:0px; margin:0px"),
        fluidRow(plotOutput("nPlot", width = "400px", height = "475px"), style = "padding:0px; margin:0px"),
      ),
      column(
        2,
        htmlOutput("pickOrder")
      )
    )
  )
)





server <- function(input, output, session) {

  # reactive vals
  obj <- reactiveValues()
  obj$n <- 0
  obj$last_exec <- Sys.time()
  obj$last_write <- Sys.time()


  dat_picks <- read.csv(here("data", "draft_order.csv"))
  obj$dat_picks <- dat_picks
  ## SETUP PICK ORDER
  first_na <- dat_picks %>% filter(is.na(player_id) & !is.na(pick)) %>% head(1)
  cat(sprintf('First pick: %s', first_na$pick), sep = '\n')
  cat(sprintf('First round: %s', first_na$round), sep = '\n')
  # first_na$pick
  obj$curr_round <- first_na$round
  obj$curr_pick <- first_na$pick

  observe({
    # redo every 5 seconds
    invalidateLater(5000)
    if (Sys.time() - obj$last_exec >= 5) {
      # load in reactive data
      comment_data <- as_tibble(isolate(fileReaderData()))

      # refresh draft order

      # only in window
      comment_data <- comment_data %>%
        filter(as.numeric(Sys.time()) - as.numeric(timestamp) <= 120)
        # filter(as.numeric(Sys.time()) - timestamp <= (5 * 60))

      # get moving average
      var_time <- as.character(5 * obj$n)
      mean_sent <- left_join(
        dplyr::select(nfl_teams, team),
        (comment_data %>%
          group_by(team) %>%
          summarise(sentiment = mean(sentiment))),
        by = 'team'
      ) %>%
        mutate(
          sentiment = replace_na(sentiment, 0),
          time = var_time
        )

      # put in tables
      if (obj$n == 0) {
        # moving average table
        obj$ma_table <- mean_sent

        # update n
        obj$n <- 1
      } else {
        # moving average table
        obj$ma_table <- rbind(obj$ma_table, mean_sent)

        # update n
        obj$n <- obj$n + 1
      }
      obj$last_exec <- Sys.time()

      # put in plot table
      curr_time <- max(as.numeric(obj$ma_table$time))
      obj$plot_table <- filter(obj$ma_table, as.numeric(time) >= (obj$n * 5) - 600) %>%
        mutate(time = as.numeric(time) - curr_time) %>%
        left_join(nfl_teams)

      # # write to file every 5 minutes
      if (Sys.time() - obj$last_exec >= 600) {
        write.csv(obj$ma_table, here("data", "ma_table.csv"), row.names = FALSE)
      }
      # # write to file every 1 minute
      # if (Sys.time() - obj$last_exec >= 60) {
      #   write.csv(obj$ma_table, here("data", "ma_table.csv"), row.names = FALSE, append = TRUE)
      # }
    }
  })

  ######################################
  ####### LINE CHART BY DIVISION #######
  ######################################
  output$maPlot <- renderPlot({
    req(obj$plot_table)

    # get time
    t <- Sys.time()
    t_string <- strftime(t, "%I:%M %p")

    # plot
    p <- ggplot(obj$plot_table, aes(x = time, y = sentiment, group = team)) +
      geom_line(aes(color = primary), size = 1) +
      # geom_image(data=filter(obj$plot_table,time==0),aes(image=logo), hjust=5, asp = 5) +
      # geom_text_repel(data=filter(obj$plot_table,time==0),aes(label=team),direction = "y", hjust = 0) +
      geom_grob(data = filter(obj$plot_table, time == 0), aes(label = logo_grob, time, sentiment), vp.height = .3, vp.width = .3) +
      scale_color_identity() +
      scale_x_continuous(limits = c(-600, 100), breaks = seq(-600, 0, by = 180), labels = c("10m", "7m", "4m", "1m")) +
      scale_y_continuous(limits = c(-1, 1)) +
      geom_hline(yintercept = 0, linetype = "longdash") +
      # scale_y_continuous(limits = 0,4, breaks = seq(0,4)) +
      facet_wrap(~division, nrow = 2) +
      theme_minimal() +
      labs(
        title = "Rolling average of Reddit comment sentiment by fanbase",
        caption = "",
        y = "mean sentiment (prior 5 minutes)",
        x = paste("minutes before", t_string)
      ) +
      theme(
        legend.position = "none",
        text = element_text(family = font)
      )
    print(p)
  })


  ######################################
  ########## BAR PLOT BY TEAM ##########
  ######################################
  output$sentPlot <- renderPlot({

    # load in reactive data
    comment_data <- as_tibble(fileReaderData()) %>%
      # filter(as.numeric(Sys.time()) - timestamp <= 120) %>%
      filter(as.numeric(Sys.time()) - timestamp <= (5 * 60)) %>%
      drop_na() %>%
      filter(team != "") %>%
      group_by(team) %>%
      summarise(sentiment = mean(sentiment), n = n()) %>%
      right_join(dplyr::select(nfl_teams, team, primary)) %>%
      mutate(sentiment = replace_na(sentiment, 0)) %>%
      left_join(nfl_teams) %>%
      mutate(team = fct_reorder(team, sentiment))

    p <- ggplot(comment_data, aes(x = sentiment, y = team)) +
      geom_col(aes(fill = primary)) +
      geom_grob(aes(label = logo_grob, sentiment, team), vp.height = .1, vp.width = .1) +
      scale_fill_identity() +
      geom_vline(xintercept = 0, linetype = "longdash", alpha = 0.5) +
      scale_x_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1), labels = c("negative", "neutral", "positive")) +
      scale_y_discrete(expand = c(0, 1)) +
      theme_minimal() +
      labs(
        title = "Mean comment sentiment (prior 5 minutes)",
        subtitle = "",
        caption = "",
        x = "mean sentiment",
        y = ""
      ) +
      theme(
        legend.position = "none",
        text = element_text(family = font),
        panel.grid.major.y = element_blank()
      )

    print(p)
  })

  ######################################
  ####### COMMENT COUNT BY TEAM ########
  ######################################
  output$nPlot <- renderPlot({

    # load in reactive data
    comment_data <- as_tibble(fileReaderData()) %>%
      # filter(as.numeric(Sys.time()) - timestamp <= 120) %>%
      filter(as.numeric(Sys.time()) - timestamp <= (5 * 60)) %>%
      drop_na() %>%
      filter(team != "") %>%
      group_by(team) %>%
      summarise(n = n()) %>%
      right_join(dplyr::select(nfl_teams, team, primary, logo_grob)) %>%
      mutate(n = as.numeric(replace_na(n, 0))) %>%
      mutate(team = fct_reorder(team, n))

    p <- ggplot(comment_data, aes(x = n, y = team)) +
      geom_col(aes(fill = primary)) +
      geom_grob(aes(label = logo_grob, n, team), vp.height = .1, vp.width = .1) +
      scale_fill_identity() +
      scale_y_discrete(expand = c(0, 1)) +
      geom_vline(xintercept = mean(comment_data$n), linetype = "longdash", alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "Number of comments (prior 5 minutes)",
        x = "number of comments",
        y = ""
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(family = font),
        panel.grid.major.y = element_blank()
      )

    print(p)
  })

  ######################################
  ############# GAUGE PLOT #############
  ######################################
  # credit to stack overflow: https://stackoverflow.com/questions/50042214/fill-a-polygon-with-gradient-scale-in-r
  output$gaugePlot <- renderPlot({
    # get team
    curr_team <- filter(obj$dat_picks, pick == obj$curr_pick)$team

    # get data
    sent <- as_tibble(fileReaderData()) %>%
      # filter(as.numeric(Sys.time()) - timestamp <= 120) %>%
      filter(as.numeric(Sys.time()) - timestamp <= (5 * 60)) %>%
      drop_na() %>%
      filter(team == curr_team) %>%
      summarize(sent = sum(sentiment)) %>%
      pull(sent)

    # avoid div by 0
    if (is.na(sent)) {
      sent <- 0
    }

    # scale sent to 0-100 scale
    sent_scale <- ((sent + 10) / 20) * 100
    if (sent_scale > 100) {
      sent_scale <- 100
    } else if (sent_scale < 0) {
      sent_scale <- 0
    }


    # plot
    p <- gg.gauge(pos = sent_scale, determinent = round(sent, 1), team_name = curr_team)
    print(p)
  })

  output$gaugePlot2 <- renderPlot({
    if (obj$curr_pick == 1) {
      boo <- as_tibble(fileReaderData()) %>%
        # filter(as.numeric(Sys.time()) - timestamp <= 30, boo == 1) %>%
        filter(as.numeric(Sys.time()) - timestamp <= (5 * 60), boo == 1) %>%
        drop_na() %>%
        count()

      boo <- min(unlist(c(100, boo)))

      # plot
      p <- gg.gauge2(pos = boo, determinent = boo)
      print(p)
    } else {
      # temporary
      prev_team <- filter(obj$dat_picks, pick == obj$curr_pick - 1)$team

      # get data
      sent <- as_tibble(fileReaderData()) %>%
        # filter(as.numeric(Sys.time()) - timestamp <= 30) %>%
        filter(as.numeric(Sys.time()) - timestamp <= (5 * 60)) %>% # , boo == 1) %>%
        drop_na() %>%
        filter(team == prev_team) %>%
        summarize(sent = sum(sentiment)) %>%
        pull(sent)

      # avoid div by 0
      if (is.na(sent)) {
        sent <- 0
      }

      # scale sent to 0-100 scale
      sent_scale <- ((sent + 10) / 20) * 100
      if (sent_scale > 100) {
        sent_scale <- 100
      } else if (sent_scale < 0) {
        sent_scale <- 0
      }


      # plot
      p <- gg.gauge(pos = sent_scale, determinent = round(sent, 1), team_name = prev_team)
      print(p)
    }
  })
  

  ######################################
  ############ READ IN FILE ############
  ######################################
  fileReaderData <- reactiveFileReader(
    8000,
    session,
    here("data", "comments.csv"),
    f_read
  )

  output$fileReaderText <- renderText({
    dat <- as_tibble(fileReaderData())
    paste("number of observations:", nrow(dat))
  })


  ######################################
  ####### STREAM IN NEW COMMENTS #######
  ######################################
  output$recentComments <- renderText({
    # get time
    t <- Sys.time()
    t_string <- strftime(t, "%I:%M %p")

    is_joke <- round(runif(1), 2) == 0.42
    dat_filt <- as_tibble(fileReaderData()) %>%
      left_join(logos) %>%
      tail(14)
    
    if(is_joke) {
      dat_filt$body <- 'Nick Wan is a fraud.'
    }
    
    dat <-
      dat_filt %>% 
      mutate(
        time = as_datetime(timestamp),
        time = strftime(time, "%I:%M %p")
      ) %>%
      mutate(
        img_html = paste("<img src='", team_logo, "' width='20'>", sep = ""),
        body = str_trunc(body, 140, side = "right"),
        text = paste(time, img_html, body)
      ) %>%
      arrange(-timestamp)
    
    # dat1 <- dat %>% head(1)
    # cat(glue::glue('{Sys.time()}: Last number is {str_sub(dat1$timestamp, -1)}'), sep = '\n')
    # if(as.integer(str_sub(dat1$timestamp, -1)) <= 3) {
    #   dat1$text <- 'Nick Wan is a fraud'
    #   dat <- bind_rows(dat1, dat %>% tail(13))
    # }
    HTML(paste("<h4><b>Recent Comments</b></h4><hr>\n", paste(dat$text, "<br/> <br/>", collapse = "")))
  })

  ######################################
  ######### INFO ON NEXT PICK ##########
  ######################################
  output$gaugeInfo <- renderText({
    # get team
    curr_team <- filter(obj$dat_picks, pick == obj$curr_pick)$team
    team_name <- filter(teamcolors, league == "nfl", mascot == curr_team) %>% pull(name)

    # get pick number
    pick_round <- obj$curr_round
    pick_number <- obj$curr_pick

    # get time
    t <- format(Sys.time())
    t_str <- strsplit(t, " ")[[1]][2] %>% substr(1, 5)
    t_html <- paste("<center><h1>", t_str, "</h1></center>")

    # get team logo
    team_logo <- filter(nfl_teams, team == curr_team) %>% pull(team_logo)
    img_html <- paste("<img src='", team_logo, "' width='20'>", sep = "")


    # get latest comment
    # comment <- as_tibble(fileReaderData()) %>%
    #     left_join(logos) %>%
    #     filter(team == curr_team) %>%
    #     tail(1) %>%
    #     mutate(time = as.character(as_datetime(timestamp-14400))) %>%
    #     separate(time, into = c("ymd","hms"), sep=" ") %>%
    #     mutate(img_html = paste("<img src='",team_logo,"' width='20'>",sep=""),
    #            body = str_trunc(body, 140, side="right"),
    #            text = paste(substr(as_hms(hms),1,5), img_html, body))

    # setup header
    team1_header <- paste(
      "</br><i>Coming up</i> <h3>Round ",
      pick_round,
      ", Pick ",
      pick_number,
      "</h3> \n <h4>",
      team_name,
      " ",
      img_html,
      "</h4>",
      sep = ""
    )

    HTML(team1_header)
    # HTML(paste(team1_header,"</br>",comment))
  })

  output$gaugeInfo2 <- renderText({
    # wait till at least pick #2
    if (obj$curr_pick == 1) {
      header <- paste("<h1>Goodell Boo-Meter</h1>", "<h4>Total boos across all fans</h4><h6>(over prior 5 minutes)</h6>")
      HTML(header)
    } else {
      # get team
      prev_team <- filter(obj$dat_picks, pick == obj$curr_pick - 1)$team
      prev_team_name <- filter(teamcolors, league == "nfl", mascot == prev_team) %>% pull(name)

      # get pick number
      pick_round <- obj$curr_round
      pick_number <- obj$curr_pick - 1

      # get time
      t <- format(Sys.time())
      t_str <- strsplit(t, " ")[[1]][2] %>% substr(1, 5)
      t_html <- paste("<center><h1>", t_str, "</h1></center>")

      # get team logo
      prev_team_logo <- filter(nfl_teams, team == prev_team) %>% pull(team_logo)
      prev_img_html <- paste("<img src='", prev_team_logo, "' width='20'>", sep = "")

      # # get player selected
      player <- filter(obj$dat_picks, pick == obj$curr_pick - 1)$player
      player <- ifelse(is.na(player), 'Hit the refresh button, Nick', player)

      # setup header
      team2_header <- paste(
        "<i>Previous pick</i> <h3>Round ",
        pick_round,
        ", Pick ",
        pick_number,
        "</h3> \n <h4>",
        prev_team_name,
        " ",
        prev_img_html,
        "</h4>",
        "</br> <h4>Selected: </h4> <b>",
        player,
        "</b>",
        sep = ""
      )

      HTML(team2_header)
    }
  })


  ######################################
  ########## PICK ORDER TABLE ##########
  ######################################
  output$pickOrder <- renderText({
    # setup image strings
    dat_init <- obj$dat_picks %>%
      left_join(logos) %>% 
      filter(pick >= (obj$curr_pick - 2))
    
    dat <-
      dat_init %>% 
      mutate(pick_html = if_else(
        pick >= obj$curr_pick,
        paste("<h4><b>Pick ", pick, ":</b> ", team, " <img src='", team_logo, "' width='20'> </br></h4>", sep = ""),
        paste("<del><h4><b>Pick ", pick, ":</b> ", team, " <img src='", team_logo, "' width='20'> </br></h4></del>", sep = "")
      ))
    HTML(paste("<h3><i>Draft Order</i></h3>", paste(dat$pick_html, collapse = ""), "</br><h6>Built by @CaioBrighenti </br> Data from PRAW</h6>"))
  })

  observeEvent(input$pickrefresh, {
    #A obj$dat_picks <- read_csv(here("data", "draft_order.csv"))
    obj$dat_picks <- refresh_draft_order()
  })

  ######################################
  ############ PICK BUTTONS ############
  ######################################
  observeEvent(input$pickback, {
    req(obj$curr_pick > 1)

    # go back one pick
    obj$curr_pick <- obj$curr_pick - 1
  })

  observeEvent(input$picknext, {
    # req(obj$curr_pick < 32)

    # go back one pick
    obj$curr_pick <- obj$curr_pick + 1
  })
}

# Run the application
shinyApp(ui = ui, server = server)
