staircase.plot            package:plotrix            R Documentation

_D_i_s_p_l_a_y _a _s_t_a_i_r_c_a_s_e _p_l_o_t.

_D_e_s_c_r_i_p_t_i_o_n:

     Displays a plot showing a sequence of changing totals and
     increments as successive linked bars.

_U_s_a_g_e:

      staircase.plot(heights,totals=NA,labels=NULL,halfwidth=0.3,main="",
      mar=NA,total.col="blue",inc.col=NA,bg.col=NA,direction="e",las=1,
      display.height=TRUE,stagger=FALSE,...)

_A_r_g_u_m_e_n_t_s:

 heights: vector of numeric values or a matrix or data frame with at
          least two columns. The first column must be numeric and the
          second may be numeric or logical.

  totals: A vector of logicals or zero/non-zero values indicating
          whether the corresponding height is a total (TRUE) or an
          increment (FALSE).

  labels: An optional vector of labels for the bars.

halfwidth: Half of the width of a bar as a proportion. See Details.

    main: A title for the plot.

     mar: Margins for the plot. Defaults to 10 on the baseline axis, 3
          on the top and 1 on the other two sides.

total.col: Color(s) for the bars representing successive totals.

 inc.col: Color(s) for the bars representing increments.

  bg.col: The background color for the plot.

direction: Direction in which the bars should be presented. See
          Details.

     las: Orientation for the bar labels. See 'par'.

display.height: Whether to display the totals and increments at the
          upper ends of the bars. Defaults to TRUE.

 stagger: Whether to stagger the labels to avoid overlap.

     ...: arguments passed to 'plot'.

_D_e_t_a_i_l_s:

     Displays a plot representing successive changes in counts or
     values. For example, if a research study attempts to contact a
     certain number of people and some cannot be contacted, some
     decline to participate, some are ineligible, the final sample will
     be smaller than the initial contact list. The first value will be
     the total of attempts, there will be a number of decrements, and
     the last value will be the actual sample. There may be
     intermediate totals specified. This produces a visual display of
     the sampling procedure. See the example.

     The bars are placed at integer values on the axis representing the
     succession of counts or values. The width of the bars is
     determined by the argument 'halfwidth'. This defaults to 0.3,
     meaning that the bar extends 0.3 to each side, so that the
     proportion of bar to space is 0.6 to 0.4. The succession of bars
     is determined by the 'direction' argument. The default is "e"
     (east), meaning that the first bar is at the left of the plot and
     subsequent bars are placed to the right. The other three
     possibilities follow the conventional compass layout.

_V_a_l_u_e:

     nil

_A_u_t_h_o_r(_s):

     Jim Lemon

_S_e_e _A_l_s_o:

     'plot'

_E_x_a_m_p_l_e_s:

      sample_size<-c(500,-72,428,-94,334,-45,289)
      totals<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE)
      labels<-c("Contact list","Uncontactable","","Declined","","Ineligible",
       "Final sample")
      staircase.plot(sample_size,totals,labels,main="Acquisition of the sample",
       total.col="gray",inc.col=2:4,bg.col="#eeeebb",direction="s")

