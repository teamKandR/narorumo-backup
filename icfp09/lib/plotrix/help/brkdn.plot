brkdn.plot              package:plotrix              R Documentation

_A _p_o_i_n_t/_l_i_n_e _p_l_o_t_t_i_n_g _r_o_u_t_i_n_e

_D_e_s_c_r_i_p_t_i_o_n:

     Display a point/line plot of breakdowns of one or more variables.

_U_s_a_g_e:

      brkdn.plot(vars,groups=NA,obs=NA,data,mct="mean",md="std.error",stagger=NA,
      dispbar=TRUE,main="Breakdown plot",xlab=NA,ylab=NA,xaxlab=NA,
      ylim=NA,type="b",pch=1,lty=1,col=par("fg"),staxx=FALSE,...)

_A_r_g_u_m_e_n_t_s:

    vars: The names or indices of one or more columns in a data frame.
          The columns must contain numeric data.

  groups: The name or index of a column in a data frame that classifies
          the values in 'vars' into different, usually fixed effect,
          levels.

     obs: The name or index of a column in a data frame that classifies
          the values in 'vars' into different, usually random effect,
          levels.

    data: The data frame.

     mct: The measure of central tendency to calculate for each group.

      md: The measure of dispersion to calculate, NA for none.

 stagger: The amount to offset the successive values at each horizontal
          position as a proportion of the width of the plot. The
          calculated default is usually adequate. Pass zero for none.

 dispbar: Whether to display the measures of dispersion as bars.

    main: The title at the top of the plot.

xlab,ylab: The labels for the X and Y axes respectively. There are
          defaults, but they are basic.

  xaxlab: Optional labels for the horizontal axis ticks.

    ylim: Optional vertical limits for the plot.

    type: Whether to plot symbols, lines or both (as in 'plot').

     pch: Symbol(s) to plot.

     lty: Line type(s) to plot.

     col: Color(s) for the symbols and lines.

   staxx: Whether to call 'staxlab' to display the X axis labels.

     ...: additional arguments passed to 'plot'.

_D_e_t_a_i_l_s:

     'brkdn.plot' displays a plot useful for visualizing the breakdown
     of a response measure by two factors, or more than one response
     measure by either a factor representing something like levels of
     treatment ('groups') or something like repeated observations
     ('obs'). For example, if observations are made at different times
     on data objects that receive different treatments, the 'groups'
     factor will display the measures of central tendency as
     points/lines with the same color, symbol and line type, while the
     'obs' factor will be represented as horizontal positions on the
     plot. If 'obs' is numeric, its unique values will be used as the 
     positions, if not, 1 to the number of unique values. This is a
     common way of  representing changes over time intervals for
     experimental groups.

_V_a_l_u_e:

     A list of two matrices of dimension 'length(levels(groups))' by 
     'length(levels(obs))'. The first contains the measures of central
     tendency calculated and its name is the name of the function
     passed as 'mct'. The second contains the measures of dispersion
     and its name is the name of the function passed as 'md'.

     If both 'groups' and 'obs' are not NA, the rows of each matrix
     will be the 'groups' and the columns the 'obs'. If 'obs' is NA,
     the rows will be the 'groups' and the columns the 'vars'. If
     'groups' is NA, the rows will be the 'vars' and the columns the
     'obs'. That is, if 'vars' has more than one element, if 'obs' is
     NA, the elements of 'vars' will be considered to represent
     observations, while if 'groups' is NA, they will be considered to
     represent groups. At least one of 'groups' and 'obs' must be not
     NA or there is no point in using 'brkdn.plot'.

_A_u_t_h_o_r(_s):

     Jim Lemon

_S_e_e _A_l_s_o:

     'dispbars'

_E_x_a_m_p_l_e_s:

      test.df<-data.frame(a=rnorm(80)+4,b=rnorm(80)+4,c=rep(LETTERS[1:4],each=20),
       d=rep(rep(letters[1:4],each=4),5))
      # first use the default values
      brkdn.plot("a","c","d",test.df,pch=1:4,col=1:4)
      # now jazz it up a bit using medians and median absolute deviations
      # and some enhancements
      bp<-brkdn.plot("a","c","d",test.df,main="Test of the breakdown plot",
       mct="median",md="mad",xlab="Temperature range", ylab="Cognition",
       xaxlab=c("10-15","16-20","21-25","25-30"),pch=1:4,lty=1:4,col=1:4)
      es<-emptyspace(bp)
      legend(es,legend=c("Sydney","Gosford","Karuah","Brisbane"),pch=1:4,
       col=1:4,lty=1:4,xjust=0.5,yjust=0.5)

