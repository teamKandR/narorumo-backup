qt.plot               package:plotrix               R Documentation

_Q_u_a_n_t_i_t_y _b_y _i_n_t_e_r_v_a_l _p_l_o_t

_D_e_s_c_r_i_p_t_i_o_n:

     Display the counts of numeric quantities against the intervals
     between  quantities that are equal when rounded to integers.

_U_s_a_g_e:

      qt.plot(qnt,qtime=NA,col=NULL,border="lightgray",
       main="Quantity x interval",xlab="Interval",ylab="Quantity",
       mar=c(5,4,4,4),...)

_A_r_g_u_m_e_n_t_s:

     qnt: Numeric vector

   qtime: Numeric vector - may be a date as an integer.

     col: The colors to fill the strips. NA for none.

  border: border color for the polygons

    main: The title of the plot.

xlab,ylab: Axis labels.

     mar: margins for the plot - defaults to leave space for scale

     ...: additional arguments passed to 'plot'.

_D_e_t_a_i_l_s:

     The intervals calculated from 'qtime' are the x values and the
     counts of values of 'qnt' are the y values of the plot displayed
     as the widths of sections of polygons running across the time
     intervals. This plot was devised to display the distribution of
     drinking, but may be useful for any situation in which it is
     desired to display the distribution of numerically coded
     quantities against the intervals between their occurrence. Note
     that if there are many values and many intervals, the resulting
     plot will be mostly empty. Categorizing the values and intervals
     so that there are only three or four categories will often produce
     a more informative plot.

     'qt.plot' assumes that the values in 'qtime' represent
     interpretable intervals like seconds or days. The default is to
     assume sequential time intervals. If 'qtime' contains dates, they
     must be translated to numeric format. These values will be sorted
     by the function. If 'qtime' is NA, it will be assigned
     '1:length(qnt)'.

_V_a_l_u_e:

     nil

_A_u_t_h_o_r(_s):

     Jim Lemon

_S_e_e _A_l_s_o:

     'polygon'

_E_x_a_m_p_l_e_s:

      # first a moderate drinker with frequent bigger sessions
      qnt<-sample(0:5,365,TRUE,prob=c(0.02,0.1,0.4,0.3,0.1,0.08))
      qtdates<-seq(as.Date("2007-01-01"),as.Date("2007-12-31"),by=1)
      qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
       ylab="Standard drinks per session")
      # now add monthly bigger sessions and notice how this
      qnt[c(30,60,90,120,150,180,210,240,270,300,330,360)]<-rep(4:5,length.out=12)
      qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
       ylab="Standard drinks per session")

