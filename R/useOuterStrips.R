#' @export
useOuterStrips <-
    function(x,
             strip = strip.default,
             strip.left = strip.custom(horizontal = FALSE),
             strip.lines = 1,
             strip.left.lines = strip.lines)
{
    dimx <- dim(x)
    stopifnot(inherits(x, "trellis"))
    stopifnot(length(dimx) == 2)
    opar <- if (is.null(x$par.settings)) list() else x$par.settings
    par.settings <-
        modifyList(opar,
                   list(layout.heights =
                        if (x$as.table) list(strip = c(strip.lines, rep(0, dimx[2]-1)))
                        else list(strip = c(rep(0, dimx[2]-1), strip.lines)),
                        layout.widths =
                        list(strip.left = c(strip.left.lines, rep(0, dimx[1]-1)))))
    if (is.character(strip))
        strip <- get(strip)
    if (is.logical(strip) && strip)
        strip <- strip.default
    new.strip <-
        if (is.function(strip))
        {
            function(which.given, which.panel, var.name, ...) {
                if (which.given == 1)
                    strip(which.given = 1,
                          which.panel = which.panel[1],
                          var.name = var.name[1],
                          ...)
            }
        }
        else strip
    if (is.character(strip.left))
        strip.left <- get(strip.left)
    if (is.logical(strip.left) && strip.left)
        strip.left <- strip.custom(horizontal = FALSE)
    new.strip.left <-
        if (is.function(strip.left))
        {
            function(which.given, which.panel, var.name, ...) {
                if (which.given == 2)
                    strip.left(which.given = 1,
                               which.panel = which.panel[2],
                               var.name = var.name[2],
                               ...)
            }
        }
        else strip.left
    update(x,
           par.settings = par.settings,
           strip = new.strip,
           strip.left = new.strip.left,
           par.strip.text = list(lines = 0.5),
           layout = dimx)
}

strip.custom <-
    function(...)
{
    args <- list(...)
    function(...)
    {
        dots <- list(...)
        do.call("strip.default",
                updateList(dots, args))
    }
}



strip.default <-
    function(which.given,
             which.panel,
##              packet.number,
##              panel.number,
             var.name,
             factor.levels,
             shingle.intervals = NULL,
             strip.names = c(FALSE, TRUE),
             strip.levels = c(TRUE, FALSE),
             sep = " : ",
             style = 1,
             horizontal = TRUE,
             ## FIXME: not sure how to incorporate alpha in strip colors
             bg = trellis.par.get("strip.background")$col[which.given],
             fg = trellis.par.get("strip.shingle")$col[which.given],
             par.strip.text = trellis.par.get("add.text"))
{
    if (horizontal)
        pushViewport(viewport(y = (which.given-0.5)/length(which.panel),
                              height = 1/length(which.panel),
                              clip = trellis.par.get("clip")$strip,
                              name = paste("strip.default", which.given, sep = ".")))
    else 
        pushViewport(viewport(x = 1 - (which.given-0.5)/length(which.panel),
                              width = 1/length(which.panel),
                              clip = trellis.par.get("clip")$strip,
                              name = paste("strip.default", which.given, sep = ".")))

    gp.text <- 
        gpar(col = par.strip.text$col,
             alpha = par.strip.text$alpha,
             lineheight = par.strip.text$lineheight,
             fontfamily = par.strip.text$fontfamily,
             fontface = chooseFace(par.strip.text$fontface, par.strip.text$font),
             cex = par.strip.text$cex)

    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length.out = 2)
    strip.levels <- rep(strip.levels, length.out = 2)
    ## str(shingle.intervals)

    formatLabel <-
        function(s,
                 abbreviate = par.strip.text$abbr,
                 minlength = par.strip.text$minl,
                 dot = par.strip.text$dot)
    {
        if (is.null(abbreviate)) abbreviate <- FALSE
        if (is.null(minlength)) minlength <- 4
        if (is.null(dot)) dot <- FALSE
        if (abbreviate) abbreviate(s, minlength = minlength, dot = dot)
        else s
    }
    factor.levels <- formatLabel(factor.levels)

    if (!is.null(shingle.intervals))
    {

        ## This usually indicates shingles, as opposed to factors.
        ## 'style' will be completely ignored, and shingle.intervals
        ## encoded using bg and fg.  Names and levels are both game.

        grid.rect(gp = gpar(fill = bg, col = bg))

        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level,]) - t[1]) / diff(t)
        if (horizontal)
            grid.rect(x = unit(r %*% c(.5,.5),"npc"),
                      width = max(unit(c(diff(r), 1), c("npc", "mm"))),
                      gp = gpar(col = fg, fill = fg))
        else 
            grid.rect(y = unit(r %*% c(.5,.5),"npc"),
                      height = max(unit( c(diff(r), 1), c("npc", "mm"))),
                      gp = gpar(col = fg, fill = fg))

        paste.and.draw(name, factor.levels[level],
                       sep = sep,
                       horizontal = horizontal,
                       showl = strip.names[2],
                       showr = strip.levels[2],
                       gp = gp.text)
    }
    else
    {
        ## Behaviour depends on 'style'.  Will separate out coloring
        ## and text based on 'style'.

        num <- length(factor.levels)

        ## coloring:

        ## background: all except style = 2
        if (style != 2) grid.rect(gp = gpar(fill = bg, col = bg))

        ## foreground: needed only for style = 2, 3 and 4

        if (num > 0 && style %in% c(2, 3, 4))
        {
            if (horizontal)
            {
                grid.rect(x = unit((2*level-1)/(2*num), "npc"),
                          width = unit(1/num, "npc"),
                          gp = gpar(fill = fg, col = fg))
            }
            else
            {
                grid.rect(y = unit((2*level-1)/(2*num), "npc"),
                          height = unit(1/num, "npc"),
                          gp = gpar(fill = fg, col = fg))
            }
        }

        ## text: [names|levels] centered only if style = 1 or 3

        if (style %in% c(1, 3))
        {
            paste.and.draw(name, factor.levels[level],
                           sep = sep,
                           horizontal = horizontal,
                           showl = strip.names[1],
                           showr = strip.levels[1],
                           gp = gp.text)
        }
        ## remaining cases
        else if (num > 0)
        {
            ## either all levels or only one
            lid <- if (style %in% c(2, 4)) 1:num else level
            if (horizontal)
            {
                grid.text(label = factor.levels[lid],
                          x = (2 * lid - 1) / (2 * num),
                          gp = gp.text)
            }
            else
            {
                grid.text(label = factor.levels[lid],
                          y = (2 * lid - 1) / (2 * num),
                          rot = 90,
                          gp = gp.text)
            }
        }
    }
    upViewport()

    ## border is drawn with clipping off
    if (horizontal)
        pushViewport(viewport(y = (which.given-0.5)/length(which.panel),
                              height = 1/length(which.panel),
                              clip = "off",
                              name = paste("strip.default.off", which.given, sep = ".")))
    else 
        pushViewport(viewport(x = 1 - (which.given-0.5)/length(which.panel),
                              width = 1/length(which.panel),
                              clip = "off",
                              name = paste("strip.default.off", which.given, sep = ".")))


    strip.border <- trellis.par.get("strip.border")
    ## draw border for strip
    grid.rect(gp =
              gpar(col = rep(strip.border$col, length.out = which.given)[which.given],
                   lty = rep(strip.border$lty, length.out = which.given)[which.given],
                   lwd = rep(strip.border$lwd, length.out = which.given)[which.given],
                   alpha = rep(strip.border$alpha, length.out = which.given)[which.given],
                   fill = "transparent"))
    upViewport()
}
