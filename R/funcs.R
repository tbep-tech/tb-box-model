show_matrixtmp <- function(avedat, txtsz = 3, trgs = NULL, yrrng = NULL, bay_segment = c('OTB', 'HB', 'MTB', 'LTB'), asreact = FALSE,
                        nrows = 10, abbrev = FALSE, family = NA, historic = FALSE, plotly = FALSE, partialyr = FALSE){

  # default targets from data file
  if(is.null(trgs))
    trgs <- targets

  # get year range from data if not provided
  if(is.null(yrrng))
    yrrng <- c(1975, max(epcdata$yr, na.rm = T))

  # process data to plot
  toplo <- anlz_attain(avedat, trgs = trgs) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::mutate(
      bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB'))
    )

  # replace calculated with historic
  if(historic){

    # 2006 to present is correct
    mans <- dplyr::tibble(
      yr = seq(1975, 2005),
      HB = c('r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'y', 'g', 'r', 'y', 'y', 'g', 'y', 'g', 'y', 'g', 'g', 'y', 'y', 'g', 'g', 'r', 'g', 'g', 'g', 'g', 'y', 'g', 'g'),
      LTB = c('g', 'y', 'r', 'y', 'r', 'r', 'r', 'r', 'r', 'y', 'y', 'g', 'g', 'g', 'y', 'y', 'y', 'y', 'y', 'r', 'y', 'g', 'y', 'r', 'y', 'y', 'y', 'g', 'y', 'y', 'y'),
      MTB = c('r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'y', 'r', 'r', 'y', 'y', 'y', 'r', 'r', 'y', 'r', 'r', 'y', 'y', 'y', 'g', 'g', 'g', 'y'),
      OTB = c('r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'y', 'r', 'r', 'g', 'y', 'y', 'y', 'r', 'y', 'y', 'r', 'y', 'g', 'y', 'y', 'r', 'r', 'g')
    ) %>%
      tidyr::gather('bay_segment', 'outcome', -yr) %>%
      dplyr::mutate(
        outcome = dplyr::case_when(
          outcome == 'g' ~ 'green',
          outcome == 'r' ~ 'red',
          outcome == 'y' ~ 'yellow'
        ),
        bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB'))
      )

    toplo <- toplo %>%
      dplyr::left_join(mans, by = c('bay_segment', 'yr')) %>%
      dplyr::mutate(
        outcome.x = dplyr::case_when(
          yr <= 2005 ~ outcome.y,
          T ~ outcome.x
        )
      ) %>%
      dplyr::select(bay_segment, yr, chl_la, outcome = outcome.x)

  }

  # add abbreviations if true
  if(abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = dplyr::case_when(
        outcome == 'red' ~ 'R',
        outcome == 'yellow' ~ 'Y',
        outcome == 'green' ~ 'G'
      )
    )
  if(!abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = outcome
    )

  # reactable object
  if(asreact){

    totab <- toplo %>%
      dplyr::select(bay_segment, yr, outcometxt) %>%
      tidyr::spread(bay_segment, outcometxt)

    colfun <- function(x){

      out <- dplyr::case_when(
        x %in% c('R', 'red') ~ '#FF3333',
        x %in% c('Y', 'yellow') ~ '#F9FF33',
        x %in% c('G', 'green') ~ '#33FF3B'
      )

      return(out)

    }


    # make reactable
    out <- show_reactable(totab, colfun, nrows = nrows)

    return(out)

  }

  # add descriptive labels, Action
  lbs <- dplyr::tibble(
    outcome = c('red', 'yellow', 'green'),
    Action = c('On Alert', 'Caution', 'Stay the Course')
  )
  toplo <- toplo %>%
    dplyr::left_join(lbs, by = 'outcome') %>%
    tidyr::separate(chl_la, c('chl', 'la'), sep = '_', remove = F) %>%
    dplyr::mutate(
      chl = paste0('chla: ', chl),
      la = paste0('la: ', la)
    ) %>%
    tidyr::unite(chl_la, c('chl', 'la'), sep = ', ') %>%
    dplyr::mutate(
      chl_la = paste0('(', chl_la, ')')
    ) %>%
    unite(Action, c('Action', 'chl_la'), sep = ' ')

  # ggplot
  p <- ggplot(toplo, aes(x = bay_segment, y = yr, fill = outcome)) +
    geom_tile(aes(group = Action), colour = 'black') +
    scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    scale_x_discrete(expand = c(0, 0), position = 'top') +
    scale_fill_manual(values = c(red = 'red', yellow = 'yellow', green = 'green')) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none'
    )

  if(!is.null(txtsz))
    p <- p +
    geom_text(aes(label = outcometxt), size = txtsz, family = family)

  if(partialyr)
    p <- p +
    labs(caption = paste0('*Incomplete data for ', max(yrrng), ' estimated\nby five year average'))

  if(plotly)
    p <- show_matrixplotly(p, family = family, tooltip = 'Action')

  return(p)

}

show_thrplottmp <- function(avedat, bay_segment = c('OTB', 'HB', 'MTB', 'LTB'), thr = c('chla', 'la'), trgs = NULL, yrrng = c(1975, 2019),
                         family = NA, labelexp = TRUE, txtlab = TRUE, thrs = FALSE, partialyr = FALSE){
  # default targets from data file
  if(is.null(trgs))
    trgs <- targets

  # yrrng must be in ascending order
  if(yrrng[1] >= yrrng[2])
    stop('yrrng argument must be in ascending order, e.g., c(1975, 2019)')

  # segment
  bay_segment <- match.arg(bay_segment)

  # wq to plot
  thr <- match.arg(thr)

  # colors
  cols <- c("Annual Mean"="red", "Management Target"="blue", "+1 se (small exceedance)"="blue", "+2 se (large exceedance)"="blue")

  # axis label
  if(labelexp)
    axlab <- dplyr::case_when(
      thr == 'chla' ~ expression("Mean Ann. Chl-a ("~ mu * "g\u00B7L"^-1 *")"),
      thr == 'la' ~ expression("Mean Ann. Light Att. (m  " ^-1 *")")
    )
  if(!labelexp)
    axlab <- dplyr::case_when(
      thr == 'chla' ~ "Mean Ann. Chl-a (ug/L)",
      thr == 'la' ~ "Mean Ann. Light Atten. (m-1)"
    )

  # get lines to plot
  toln <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment)
  trgnum <- toln %>% dplyr::pull(!!paste0(thr, '_target'))
  smlnum <- toln %>% dplyr::pull(!!paste0(thr, '_smallex'))
  thrnum <- toln %>% dplyr::pull(!!paste0(thr, '_thresh'))


  # change label location if thrs is true
  if(!thrs)
    num <- trgnum
  if(thrs)
    num <- thrnum

  # threshold label
  if(labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "~ mu * g%.%L^{-1}"),
      thr == 'la' ~ paste(num, "~m","^{-1}")
    )
  if(!labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "ug/L"),
      thr == 'la' ~ paste(num, "m-1")
    )

  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name)

  if(partialyr)
    ttl <- paste0(ttl, '*')

  # get data to plo
  toplo <- avedat$ann %>%
    dplyr::filter(grepl(paste0('_', thr, '$'), var)) %>%
    mutate(var = 'yval') %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    tidyr::spread(var, val)

  p <- ggplot() +
    geom_point(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), size = 3) +
    geom_line(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), linetype = 'solid', size = 0.75) +
    labs(y = axlab, title = ttl) +
    scale_x_continuous(breaks = seq(yrrng[1], yrrng[2], by = 1)) +
    theme(axis.title.x = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background = element_rect(fill = '#ECECEC'),
          legend.position = 'top',#c(0.85, 0.95),
          legend.background = element_rect(fill=NA),
          legend.key = element_rect(fill = '#ECECEC'),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 7, hjust = 1)
    )

  # all targets/thresholds
  if(!thrs)
    p <- p +
    geom_hline(aes(yintercept = trgnum, colour = 'Management Target')) +
    geom_hline(aes(yintercept = smlnum, colour = '+1 se (small exceedance)'), linetype = 'dashed') +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols, labels = factor(names(cols), levels = names(cols))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA, NA, NA),
        colour = cols,
        linetype = c('solid', 'solid', 'dashed', 'dotted'),
        size = c(0.75, 0.5, 0.5, 0.5)
      )
    ))

  # thresholds only
  if(thrs)
    p <- p +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols[c(1, 4)], labels = factor(names(cols[c(1, 4)]), levels = names(cols[c(1, 4)]))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA),
        colour = cols[c(1, 4)],
        linetype = c('solid', 'dotted'),
        size = c(0.75, 0.5)
      )
    ))

  if(txtlab & !thrs)
    p <- p +
    geom_text(aes(yrrng[1], num, label = trglab), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')

  if(txtlab & thrs)
    p <- p +
    geom_text(aes(yrrng[1], max(toplo$yval), label = trglab), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')


  if(partialyr)
    p <- p +
    labs(caption = paste0('*Incomplete data for ', max(yrrng), ' estimated by five year average'))

  return(p)

}

show_segmatrixtmp <- function(avedat, txtsz = 3, trgs = NULL, yrrng = c(1975, 2019), bay_segment = c('OTB', 'HB', 'MTB', 'LTB'),
                           abbrev = FALSE, family = NA, historic = FALSE, plotly = FALSE, partialyr = FALSE) {

  bay_segment <- match.arg(bay_segment)

  # outcome data
  outdat <- show_matrixtmp(avedat, bay_segment = bay_segment, txtsz = NULL, trgs = trgs, yrrng = yrrng, historic = historic, abbrev = abbrev)
  outdat <- outdat$data %>%
    dplyr::mutate(
      var = 'outcome',
      bay_segment = as.character(bay_segment)
    ) %>%
    dplyr::select(bay_segment, yr, var, Action, outcome, outcometxt)

  # chloropyll and la data
  chldat <- show_wqmatrixtmp(avedat, param = 'chl', bay_segment = bay_segment, trgs = trgs, txtsz = NULL, yrrng = yrrng, abbrev = abbrev)
  chldat <- chldat$data
  ladat <- show_wqmatrixtmp(avedat, param = 'la', bay_segment = bay_segment, trgs = trgs, txtsz = NULL, yrrng = yrrng, abbrev = abbrev)
  ladat <- ladat$data
  wqdat <- dplyr::bind_rows(chldat, ladat) %>%
    dplyr::rename(Action = Result) %>%
    dplyr::select(-Action) %>%
    dplyr::mutate(
      var = gsub('^mean\\_', '', var),
      bay_segment = as.character(bay_segment)
    )

  # outcome results for chlorophyll and la, e.g., large/small, short/long exceedances
  vals <- avedat %>%
    anlz_attain(magdurout = T) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::mutate(
      mags = factor(mags, levels = c(0, 1, 2), labels = c('none', 'small', 'large')),
      mags = as.character(mags),
      mags = paste0('Exceedance: ', mags, ' (size)'),
      durats = case_when(
        durats %in% 0 ~ 'none',
        durats %in% c(1, 2, 3) ~ 'short',
        durats %in% 4 ~ 'long'
      ),
      durats = paste0(durats, ' (length)'),
      outcome = paste0('Outcome: ', outcome)
    ) %>%
    dplyr::select(-val, -target, -smallex, -thresh) %>%
    tidyr::unite(Action, c('outcome', 'mags', 'durats'), sep = ', ')

  # combine wqdat with outcome results and outcome data
  toplo <- wqdat %>%
    dplyr::left_join(vals, by = c('bay_segment', 'yr', 'var')) %>%
    bind_rows(outdat) %>%
    dplyr::mutate(
      var = factor(var, levels = c('la', 'outcome', 'chla'), labels = c('Light attenuation', 'Management outcome', 'Chlorophyll-a'))
    )

  # create plot
  p <- ggplot(toplo, aes(x = var, y = yr, fill = outcome)) +
    geom_tile(aes(group = Action), colour = 'black') +
    scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    scale_x_discrete(expand = c(0, 0), position = 'top') +
    scale_fill_manual(values = c(red = 'red', yellow = 'yellow', green = 'green')) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none'
    )

  # text if not null
  if(!is.null(txtsz))
    p <- p +
    geom_text(aes(label = outcometxt), size = txtsz, family = family)

  if(plotly)
    p <- show_matrixplotly(p, family = family, tooltip = 'Action')

  return(p)

}

show_wqmatrixtmp <- function(avedat, param = c('chla', 'la'), txtsz = 3, trgs = NULL, yrrng = c(1975, 2019), bay_segment = c('OTB', 'HB', 'MTB', 'LTB'),
                          asreact = FALSE, nrows = 10, abbrev = FALSE, family = NA, plotly = FALSE, partialyr = FALSE){

  # sanity checks
  param <- match.arg(param)

  # default targets from data file
  if(is.null(trgs))
    trgs <- targets

  # process data to plot
  avedat <- avedat %>%
    .$ann
  toplo <- avedat %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::filter(var %in% !!paste0('mean_', param)) %>%
    dplyr::left_join(trgs, by = 'bay_segment') %>%
    dplyr::select(bay_segment, yr, var, val, thresh = !!paste0(param, '_thresh')) %>%
    dplyr::mutate(
      bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB')),
      outcome = dplyr::case_when(
        val < thresh ~ 'green',
        val >= thresh ~ 'red'
      )
    )

  if(abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = dplyr::case_when(
        outcome == 'red' ~ 'R',
        outcome == 'green' ~ 'G'
      )
    )
  if(!abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = outcome
    )


  # reactable object
  if(asreact){

    totab <- toplo %>%
      dplyr::select(bay_segment, yr, outcometxt) %>%
      tidyr::spread(bay_segment, outcometxt)

    colfun <- function(x){

      out <- dplyr::case_when(
        x %in% c('R', 'red') ~ '#FF3333',
        x %in% c('G', 'green') ~ '#33FF3B'
      )

      return(out)

    }

    # make reactable
    out <- show_reactable(totab, colfun, nrows = nrows)

    return(out)

  }

  # add descriptive labels, Result
  lbs <- dplyr::tibble(
    outcome = c('red', 'green'),
    Result = c('Above', 'Below')
  )
  if(param == 'chla')
    rndval <- 1
  if(param == 'la')
    rndval <- 2
  toplo <- toplo %>%
    dplyr::left_join(lbs, by = 'outcome') %>%
    dplyr::mutate(
      val = paste0('Average: ', round(val, rndval)),
      thresh = paste0('Threshold: ', round(thresh, rndval))
    ) %>%
    tidyr::unite(segval, c('val', 'thresh'), sep = ', ') %>%
    dplyr::mutate(
      segval = paste0('(', segval, ')')
    ) %>%
    unite(Result, c('Result', 'segval'), sep = ' ')

  # ggplot
  p <- ggplot(toplo, aes(x = bay_segment, y = yr, fill = outcome)) +
    geom_tile(aes(group = Result), colour = 'black') +
    scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    scale_x_discrete(expand = c(0, 0), position = 'top') +
    scale_fill_manual(values = c(red = 'red', green = 'green')) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none'
    )

  if(!is.null(txtsz))
    p <- p +
    geom_text(aes(label = outcometxt), size = txtsz, family = family)

  if(partialyr)
    p <- p +
    labs(caption = paste0('*Incomplete data for ', max(yrrng), ' estimated\nby five year average'))

  if(plotly)
    p <- show_matrixplotly(p, family = family, tooltip = 'Result')

  return(p)

}

show_segplotlytmp <- function(avedat, bay_segment = c('OTB', 'HB', 'MTB', 'LTB'), yrrng = c(1975, 2019), family = NULL, partialyr = FALSE, width = NULL, height = NULL){

  bay_segment <- match.arg(bay_segment)

  suppressMessages({

    p1 <- show_thrplottmp(avedat, bay_segment = bay_segment, thr = "chla", yrrng = yrrng, family = family, txtlab = F, labelexp = F, partialyr = partialyr) +
      ggtitle(NULL) +
      scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(yrrng[1], yrrng[2]))
    p2 <- show_thrplottmp(avedat, bay_segment = bay_segment, thr = "la", yrrng = yrrng, family = family, txtlab = F, labelexp = F, partialyr = partialyr) +
      ggtitle(NULL) +
      scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(yrrng[1], yrrng[2]))

    p3 <- show_segmatrixtmp(avedat, bay_segment = bay_segment, yrrng = yrrng, txtsz = NULL, partialyr = partialyr) +
      scale_y_continuous(expand = c(0,0), breaks = c(yrrng[1]:yrrng[2])) +
      coord_flip() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text = element_text(size = 12),
        text = element_text(family = family)
      )

  })

  p3 <- plotly::ggplotly(p3, tooltip = 'Action')
  for(i in 1:length(p3$x$data)) p3$x$data[[i]]$showlegend <- FALSE

  p1 <- plotly::ggplotly(p1, width = width, height = height)
  p2 <- plotly::ggplotly(p2, width = width, height = height)
  p2$x$data[[1]]$showlegend <- FALSE
  p2$x$data[[2]]$showlegend <- FALSE
  p2$x$data[[3]]$showlegend <- FALSE
  p2$x$data[[4]]$showlegend <- FALSE

  # remove unnecessary hover text
  p1$x$data[[1]]$text <- gsub('colour:\\sAnnual\\sMean$', '', p1$x$data[[1]]$text)
  p2$x$data[[1]]$text <- gsub('colour:\\sAnnual\\sMean$', '', p2$x$data[[1]]$text)

  out <- plotly::subplot(p1, p3, p2, nrows = 3, heights = c(0.4, 0.2, 0.4), shareX = T, titleY = TRUE)

  return(out)

}
