#' One- or Two- Column Foresplot of Biomarker Associations
#'
#' This function reads three dataframes, a biomarker association dataframe, its corresponding standard error dataframe and its corresponding p-value dataframe and plots a custom-size and layout, 1- or 2- column, forest plot.
#'
#' The parameters \code{biomarker_groups_as_list} and \code{indices} allow for customization of the layout in 1- or 2- column, 1- or 2- page forestplots. Specifically, if the indices is specified (as opposed to the default NULL) it must be a list of numeric vectors, that has either 1, 2 or 4 components, which allow for a 1-column/1-page, 2-column/1-page or 2-column/2-page forestplot, respectively (see examples). The list essentially defines the rows from beta that will be plotted. By adding NA in selected positions of the biomarker_groups_as_list while increasing the number of indices, accordingly, one can add extra white space between biomarker of biomarker categories.
#'
#'
#' @param beta A data frame (either \code{tibble} or not) containing named columns in the following way: the first column must be the exact abbreviations of the NMR biomarkers (see built-in \code{biomarkers} dataset). This column must be named "abbrev". The rest of the columns, i.e. the study columns, must contain associations and they may be named with the name of the study, e.g. if the column contains the univariate associations of the biomarkers to BMI, the column may be named BMI. Use more than one study columns if you want to plot more than one studies in the same file. Avoid plotting more than 5 or 6 studies together, cause the result is not pretty and/or may be hard to read. It is important that the columns have all either linear associations or odds/hazard ratios. The reason is that odds/hazard ratios are plotted on a log axis, whereas linear associations not.
#' @param se A data frame (either \code{tibble} or not) in the same format as beta parameter. Keep the same order of columns and preferably the same order of rows (although the latter is not necessary).
#' @param pval A data frame (either \code{tibble} or not) in the same format as beta and se parameters. Keep the same order of columns and preferably the same order of rows (although the latter is not necessary).
#' @param biomarker_groups_as_list A named list of character vactors containing the groups of biomarkers to plot (see examples). The category names, i.e. the names of the components of the list, can be anything. The actual character vectors must contain the exact biomarker abbreviations. See Results.tsv or Results.xlsx files or the built-in dataset biomarkers$abbrev.
#' @param indices Either NULL or a list of numeric vectors, that has either 1, 2 or 4 components containing the rows from beta that will eventually be plotted. It basically allows to customize the layout of the forestplot. If NULL and biomarker_groups_as_list has all serum or plasma biomarkers then a 2-column, 2-page forestplot is printed, containing all biomarkers. If a list with 1 component, e.g. list(c(1:30)), then a 1-column, 1-page forestplot is printed, containing all the biomarkers from beta dataframe until row number tail(indices[[1]], 1), e.g. tail(list(c(1:30))[[1]], 1)=30. If a list with 2 components, then a 2-column, 1 page forestplot is printed. If a list with 4 components, then a 2-column, 2-page forestplot is printed.
#' @param filename A character with the name of the pdf file that will contain the plot. Defaults to 'forest_plot.pdf'
#' @param plot_title A character (defaults to NULL) with a title for the plot. If NULL no title.
#' @param is_log_odds_ratio logical (defaults to F) specifying whether the associations are linear or not. If TRUE, provide the log odds ratio as the function will exponentiate the betas internally. For TRUE a log scale is be used.
#' @param xlabel A character with the xlab to display. Defaults to "beta".
#' @param signif_cutoff Numeric specifying the cutoff for statistical significance. E.g. often a cutoff of 0.05 is used. Associations with values larger than that will be plotted with an empty circular point.
#' @param plotcolors A vector of characters specifying the color of the plotted points. Defaults to NULL in which case, if only one study, black is used. If more than one studies, the script generates a default palette.
#' @param plotpointshape An integer or vector of integers (default 21) signifying the shape of points used for the plot. The values must be one of 21,22,23,24 or 25 in order for the insignificant cases to be displayed as empty shapes.
#' @param legend_vars A vector of characters specifying the legend names for when more than one studies are plotted.
#' @param bottom_margin The margin from the bottom of the plot (the forestplot will be plotted in a A4 paper).
#' @param left_margin The margin from the left edge of the plot.
#' @param top_margin The margin from the top of the plot.
#' @param right_margin The margin from the right edge of the plot.
#' @param cex_text The size of the y- and x-label. Legends and titles will be adjusted with respect to that.
#' @param ylabelpos The distance of the ylabels from the plot. This parameter will most likley need to be adjusted in conjuction with the margins and the cex_tex, especially when the non-default layout is used.
#' @param biomarker_name_option numeric (defaults to option 1), currently takes values 1 (for option 1) and 2 (for option 2). The main difference between the two options is how the names of the lipoprotein subclasses are displayed. For example, option 2 will display XXL-VLDL-TG \% for the ratio of triglycerides in XXL VLDL particles, whereas option 1 assumes that plotting will be done according to lipid type, e.g. all triglycerides plotted in the same subgroup, therefore it would only display "Extremely large VLDL" (under the category "Triglycerides in lipoproteins").
#' @param ... Arguments to be passed to the \code{pdf} device, like \code{paper}, \code{width}, \code{height} e.t.c.
#' @export
#' @author Qin Wang, Maria Kalimeri
#' @note It is important that the columns have all either linear associations or odds/hazard ratios. The reason is that odds/hazard ratios are plotted on a log axis, whereas linear associations not.
#' @examples
#' bmr_all_grouped <- bmr_selected_grouping(bmr_grouping_choice = "serum_all")
#'
#' forestplot_nmr(beta=demo_beta,
#'            se=demo_se,
#'            pval=demo_pval,
#'            biomarker_groups_as_list=bmr_all_grouped,
#'            filename='plot_linear_comparison.pdf',
#'            plot_title="Linear associations to BMI",
#'            is_log_odds_ratio=FALSE,
#'            xlabel="SD difference (95% CI)",
#'            signif_cutoff=0.05,
#'            legend_vars=names(demo_beta)[2:3],
#'            paper="a4",
#'            height = 12,
#'            width = 9)

forestplot_nmr <- function(beta,
                       se,
                       pval,
                       biomarker_groups_as_list,
                       indices=NULL,
                       filename='forest_plot.pdf',
                       plot_title=NULL,
                       is_log_odds_ratio=F,
                       xlabel="beta",
                       signif_cutoff=0.05,
                       plotcolors=NULL,
                       plotpointshape=21,
                       legend_vars=NULL,
                       cex_text=NULL,
                       bottom_margin=2,
                       left_margin=NULL,
                       top_margin=2,
                       right_margin=3,
                       ylabelpos=NULL,
                       biomarker_name_option=1,
                       ...){

  # Run a check for the biomarker_name_option flag
  if (!biomarker_name_option %in% c(1,2)){
    stop("There are only two options for the biomarker_name_option parameter. Use either numeric 1 or 2")
  }
  # beta, a matrix with rows as metabolites and columns as different outcomes.
  # se and pval same as beta
  # beta, se and pVal should be the same size matrix and have matching rownames and colnames
  combo_result_grouped <- add_bmr_group_names(beta,
                                              se,
                                              pval,
                                              biomarker_groups_as_list)

  beta <- combo_result_grouped$beta_grouped
  se <- combo_result_grouped$se_grouped
  pVal <- combo_result_grouped$pval_grouped
  no_of_studies <- ncol(beta)-1
  #
  rowname=beta$abbrev

  # reformat the data
  # loop over all column with a result (i.e. all except for first column that has
  # the biomarker names)
  result=list()
  for (i in 1:(ncol(beta)-1)){
    if (!is_log_odds_ratio){
      ibeta=beta[,i+1]
      ise=se[,i+1]
      ci.low=ibeta-1.96*ise
      ci.up=ibeta+1.96*ise
    } else {
      ibeta=exp(beta[,i+1])
      ise=se[,i+1]
      ci.low=exp(beta[,i+1]-1.96*ise)
      ci.up=exp(beta[,i+1]+1.96*ise)
    }
    ipVal=pVal[,i+1]
    result[[i]] <- cbind(ibeta,
                         ise,
                         ipVal,
                         ci.low,
                         ci.up)
    colnames(result[[i]])=c('Estimate',
                            'Std. Error',
                            'Pr(>|t|)',
                            '2.5 %',
                            '97.5 %')
  }
  names(result) <- colnames(beta)[2:ncol(beta)]

  # Double sapply throws warnings if result has length 1, i.e. plotting 1 cohort
  # Solution, add if/else.
  errorcols <- result %>%
    sapply(function(x) x[,c("2.5 %", "97.5 %")])
  if (length(result) > 1){
    xleft <-
      errorcols %>%
      sapply(function(x) min(x, na.rm = T)) %>%
      min()

    xright <-
      errorcols %>%
      sapply(function(x) max(x, na.rm = T)) %>%
      max()

  } else if (length(result)==1){
    xleft <- min(errorcols, na.rm = T)

    xright <- max(errorcols, na.rm = T)
  }

  xrange=c(xleft, xright)
  # Check for rare case where all plotted biomarkers having a significant effect
  # in the same direction and therefore the null case (0 in linear and 1 in log)
  # is missing from the plot
  if (is_log_odds_ratio) {
    if (xrange[1] > 1) {
      xrange <- c(1-(1/xrange[2]), xrange[2])
    } else if (xrange[2] < 1) {
      xrange <- c(xrange[1], 1+(1/xrange[1]))
    }
  } else {
    if (xrange[1] > 0) {
      xrange <- c(0-xrange[2], xrange[2])
    } else if (xrange[2] < 0) {
      xrange <- c(xrange[1], 0+xrange[1])
    }
  }


  # Plot parameters
  # Divide the plot in different groups
  # The groups are defined by the components of an object called indices

  # All NG biomarkers will be plotted in 2 pages and two columns in each page
  if (!length(indices) %in% c(1,2,4, 0)){
    stop("The parameter 'indices'  must be a list of numeric vectors,
         that has either 1, 2 or 4 components")
  }
  if (is.null(indices) && nrow(beta)==260){
    indices <-
      list(first_indices=1:66,
           second_indices=67:132,
           third_indices=133:196,
           forth_indices=197:260)
    max_page <- 2
    max_plot_col <- 2
    # Specific biomarkers will be plotted in one page and one column
  } else if (is.null(indices) && nrow(beta) < 150){
    indices <-
      list(first_indices=1:nrow(beta))
    max_page <- 1
    max_plot_col <- 1
  } else if (!is.null(indices)){
    max_page <- ifelse(length(indices)==4, yes = 2, no = 1)
    max_plot_col <- length(indices)
  }
  # Aesthetics
  if (no_of_studies==1 && is.null(plotcolors)){
    plotcolors <-  "black"
  } else if (no_of_studies>1 && is.null(plotcolors)){
    plotcolors <- colorRampPalette(c(fpcolors[1], fpcolors[2], fpcolors[3]),
                                   interpolate="linear",
                                   space = "rgb")(no_of_studies)
  } else if (no_of_studies>1 && length(plotcolors)<no_of_studies){
    warning("Number of colors provided is less than the studies to plot.\n Will use built-in colors.")
    plotcolors <- colorRampPalette(c(fpcolors[1], fpcolors[2], fpcolors[3]),
                                   interpolate="linear",
                                   space = "rgb")(no_of_studies)
  }

  # Custom margins for linear/log and 1/2 columns
  if (is.null(left_margin) && is.null(ylabelpos)){
    if (!is_log_odds_ratio && max_plot_col==2){
      left_margin=5
      ylabelpos=-6
    } else if (!is_log_odds_ratio && max_plot_col==1) {
      left_margin=5
      ylabelpos=-9
    } else if (is_log_odds_ratio && max_plot_col==2) {
      left_margin=10
      ylabelpos=-0.5
    } else if (is_log_odds_ratio && max_plot_col==1) {
      left_margin=14
      ylabelpos=-0.5
    }
  }

  # Custom cex_tex size
  if (is.null(cex_text)){
    if (max_plot_col==2){
      cex_text <- 0.8
    } else {
      cex_text <- 1.1
    }
  }

  # PCH point style
  if (length(plotpointshape) == no_of_studies){
    pchh <- plotpointshape
  } else {
    pchh=rep(plotpointshape[1],no_of_studies)
  }
  if (!all(plotpointshape %in% c(21,22,23,24,25))){
    warning("plotpointshape values must be in the interval 21, 22, 23, 24 or 25
            in order for p-values > signif_cutoff to be shown as empty symbols.")
  }

  pdf(filename, pointsize = 10, ...)
  # Loop over pages
  for (paGe in 1:max_page){
    par(mar=c(bottom_margin,left_margin,top_margin,right_margin))
    if (max_plot_col==2){
      layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(29, 1))
    } else {
      layout(matrix(c(1,2), ncol=1, byrow=TRUE), heights=c(29, 1))
    }
    # Loop over plotting columns
    for (plot_col in 1:max_plot_col){
      if (paGe==1) bmridx <- indices[[plot_col]]
      if (paGe==2) bmridx <- indices[[plot_col+2]]
      estimates=sapply(result,function(x){x[bmridx,'Estimate']})
      lowerCI=sapply(result,function(x){x[bmridx,'2.5 %']})
      upperCI=sapply(result,function(x){x[bmridx,'97.5 %']})
      vp=sapply(result,function(x){x[bmridx,'Pr(>|t|)']})
      # Get the display name of the biomarker
      row_idx <-
        match(rowname[bmridx], biomarkers$abbrev)
      if (biomarker_name_option==1){
        dispname <-
          biomarkers[row_idx,] %>%
          pull(., forest_plot_disp_name1)
      } else {
        dispname <-
          biomarkers[row_idx,] %>%
          pull(., forest_plot_disp_name2)
      }

      # If the abbreviation doesn't exist, return the input
      na_idx <-
        is.na(dispname)
      dispname[na_idx] <- rowname[bmridx][na_idx]

      #the colour for the points
      colorMatrix <-
        matrix(rep(plotcolors,nrow(vp)),
               nrow = nrow(vp),
               ncol = ncol(vp),
               byrow = T)
      colorMatrix[vp>signif_cutoff]="white"

      #plot
      yv0=seq(1,by=2.5,length.out=nrow(estimates));
      yv0=rev(yv0);
      yrange=c(min(yv0)-1.25,max(yv0)+1.25)
      offset=seq(2.5,0,length.out=ncol(estimates)+2)
      offset=offset[-c(1,length(offset))]
      for (j in 1:ncol(estimates)){
        if (j==1){
          if (is_log_odds_ratio){
            plot(estimates[,j],yv0,col="transparent",xlab=NA,ylab=NA,xaxt="n",yaxt="n", log = "x",
                 xlim=c(xrange[1],xrange[2]),ylim=yrange, bty="n",yaxs='i',xpd=T);
          } else {
            plot(estimates[,j],yv0,col="transparent",xlab=NA,ylab=NA,xaxt="n",yaxt="n",
                 xlim=c(xrange[1]-abs(xrange[1])*0.7,xrange[2]),ylim=yrange, bty="n",yaxs='i',xpd=T);
          }
          nextBoxColor <- "white"
          for (k in 1:nrow(estimates)){
            if (is.na(estimates[k])){
              rect(xrange[1],yv0[k]-1.2,xrange[2],yv0[k]+1.2,col='white', border='white')
              nextBoxColor <- "grey92"
            } else {
              rect(xrange[1],yv0[k]-1.2,xrange[2],yv0[k]+1.2,col=nextBoxColor, border='transparent')
              axis(side=2,line=ylabelpos,at=yv0[k],labels=dispname[k],cex.axis=cex_text,las=1,tick=F,hadj=1);
              if (no_of_studies==1){
                axis(side=4,line=-1,at=yv0[k],labels=sprintf("%.2f", round(estimates[k,j],2)),
                     cex.axis=cex_text-0.1,las=1,tick=F,hadj=0);
              }
              if (nextBoxColor=="white"){
                nextBoxColor <- "grey92"
              } else {
                nextBoxColor <- "white"
              }
            }
          }
          #


          if (is_log_odds_ratio){
            cut=axTicks(side=1, log = T) %>%
              .[which(.>xrange[1] & .<xrange[2])]
            mid <- 1
          } else {
            cut=axTicks(side=1) %>%
              .[which(.>xrange[1] & .<xrange[2])]
            mid <- 0
          }
          cutd <- cut

          abline(v=cutd,lty=2,col="grey67",ylim=c(min(yv0)-1.25,max(yv0)+1.25));
          mtext(text = xlabel,at=c(mid), side=1, line=1.3, xpd=T, cex=cex_text+0.1)

          abline(v=mid,col="black", ylim=c(min(yv0)-1.25,max(yv0)+1.25));
          lines(c(xrange[1],xrange[2]),c(min(yv0)-1.25,min(yv0)-1.25))
          axis(side=1,line=-0.9,at=cut,labels=sprintf("%.2f", cut),tick=F,cex.axis=cex_text);
          yv=yv0-1.5+offset[j];
          yv=yv+0.25
          segments(lowerCI[,j],yv,upperCI[,j],yv,col=plotcolors[j],lwd=1.5,xpd=NA)

          # This is here (and not earlier with the similar part) cause I draw
          # the white rectangles on top of the vertical lines in order not to have vertical lines
          # all over the empty space
          for (k in 1:nrow(estimates)){
            if (is.na(estimates[k])){
              rect(xrange[1],yv0[k]-1.2,xrange[2],yv0[k]+1.2,col='white', border='white')
              axis(side=2,line=ylabelpos-2,at=yv0[k],labels=dispname[k],cex.axis=cex_text,font=2,las=1,tick=F,hadj=1);
            }
          }


          if (no_of_studies<=2) cex.p=1.1 else if (no_of_studies>2 & no_of_studies<=4) cex.p=0.8 else cex.p=0.6
          points(estimates[,j],yv,pch=pchh[j],bg=colorMatrix[,j],col=plotcolors[j],xpd=NA,lwd=1.2,cex=cex.p);
        } else
        {
          yv=yv0-1.2+offset[j];
          segments(lowerCI[,j],yv,upperCI[,j],yv,col=plotcolors[j],lwd=1.5,xpd=NA)
          points(estimates[,j],yv,pch=pchh[j],bg=colorMatrix[,j],col=plotcolors[j],xpd=NA,lwd=1.2,cex=cex.p);
        }
      }
    }

    mtext(text = plot_title,line = -1.5,side=3,cex=cex_text+0.3,outer = T)
    if (!is.null(legend_vars)){
      if (max_plot_col==2){
        legpos <- "center"
      } else {
        legpos <- "bottomleft"
      }
      par(mai=c(0,0,0,0))
      plot.new()
      legend(x=legpos,
             legend=legend_vars,
             col=plotcolors,
             pch=pchh,
             lty=1,
             border='transparent',
             lwd=1,
             pt.bg=plotcolors,
             box.col = 'transparent',
             horiz = F,
             xpd = NA,
             cex=1.1)
    }
  }
  dev.off()
  }
