# tax_intfee_2023_xx_yy.R "Should Credit Card Rewards Be Taxed"
# Model simulations and charts.
library(ggplot2); theme_set(theme_bw())
library(latex2exp)

# Model parameters:
# Tax parameters 
(delta = 0)
(delta1.vec = seq(0, 0.5, 0.01))
(delta0.vec = rep(0, length(delta1.vec)))
(tau = 0)
(tau1.vec = seq(0, 0.5, 0.01))
(tau0.vec = rep(0, length(tau1.vec)))
#
(nc = 1.2) # num of consumers
(mp = 1.2) # num of p-merchants
(me = 0.2*mp)# num of e-merchants
#
(beta_c = 2)
#(beta_p = 3)# beta_p-bph > beta_c-bch makes phi_bar > 0, i.e., positive IF
(beta_p = 2)
(beta_e = 2)
#
(bch = 1)
(bph = 1)
#
# Verify Assumption 1a
beta_c > bch
beta_p > bph
# Verify Assumption 1b
abs((1-delta1.vec)*beta_c - (1-tau0.vec)*beta_p) <= (1-delta1.vec)*bch + (1-tau0.vec)*bph
abs((1-delta0.vec)*beta_c - (1-tau1.vec)*beta_p) <= (1-delta0.vec)*bch + (1-tau1.vec)*bph

# Profit maximizing IF phi_bar equation (7) in paper
(phi_bar = ((1-tau)*(beta_p - bph) - (1-delta)* (beta_c -  bch))/(2*(1-tau)*(1-delta)) )
#
(phi_bar_delta1_tau0.vec = ((1-tau0.vec)*(beta_p - bph) - (1-delta1.vec)* (beta_c -  bch))/(2*(1-tau0.vec)*(1-delta1.vec)) )
#
(phi_bar_delta0_tau1.vec = ((1-tau1.vec)*(beta_p - bph) - (1-delta0.vec)* (beta_c -  bch))/(2*(1-tau1.vec)*(1-delta0.vec)) )

# xhat and yhat under card company IF
(xhat = (bch - (1-tau)*phi_bar)/(beta_c))
(yhat = (bph + (1-delta)*phi_bar)/(beta_p))
#
(xhat_delta1_tau0.vec = (bch - (1-tau0.vec)*phi_bar_delta1_tau0.vec)/(beta_c))
#
(yhat_delta1_tau0.vec = (bph + (1-delta1.vec)*phi_bar_delta1_tau0.vec)/(beta_p))
#
(xhat_delta0_tau1.vec = (bch - (1-tau1.vec)*phi_bar_delta0_tau1.vec)/(beta_c))
#
(yhat_delta0_tau1.vec = (bph + (1-delta0.vec)*phi_bar_delta0_tau1.vec)/(beta_p))

# Cash and card volumes under card company IF
(vh = nc*xhat*mp + nc*(1-xhat)*mp*yhat)
(vd = nc*(1-xhat)*mp*(1-yhat) + nc*me)
vh + vd == nc*(mp + me)# verify sum of payments
#
(vh_delta1_tau0.vec= nc*xhat_delta1_tau0.vec*mp + nc*(1-xhat_delta1_tau0.vec)*mp*yhat_delta1_tau0.vec)
(vd_delta1_tau0.vec = nc*(1-xhat_delta1_tau0.vec)*mp*(1-yhat_delta1_tau0.vec) + nc*me)
vh_delta1_tau0.vec + vd_delta1_tau0.vec == nc*(mp + me)# verify sum of payments
#
(vh_delta0_tau1.vec= nc*xhat_delta0_tau1.vec*mp + nc*(1-xhat_delta0_tau1.vec)*mp*yhat_delta0_tau1.vec)
(vd_delta0_tau1.vec = nc*(1-xhat_delta0_tau1.vec)*mp*(1-yhat_delta0_tau1.vec) + nc*me)
vh_delta0_tau1.vec + vd_delta0_tau1.vec == nc*(mp + me)# verify sum of payments
#

# Making a data frame
(vol_delta1.df = data.frame(delta1.vec, Xhat = xhat_delta1_tau0.vec, yhat_delta1_tau0.vec,  vd_delta1_tau0.vec))# w.r.t delta
(vol_tau1.df = data.frame(tau1.vec, Xhat = xhat_delta0_tau1.vec, yhat_delta0_tau1.vec,  vd_delta0_tau1.vec))# w.r.t tau

#
# Plot w.r.t delta (tau = 0) [not in paper]
ggplot(vol_delta1.df, aes(x=delta1.vec)) + geom_line(aes(y=vd_delta1_tau0.vec), linetype = "solid", size=1.2) + geom_line(aes(y=xhat_delta0_tau1.vec), linetype = "longdash", size=1.2) + geom_line(aes(y=yhat_delta0_tau1.vec),  linetype = "dotdash", size=1.5) + labs(x= TeX("Merchants' rate of tax deduction $\\delta$"), y= TeX("$\\hat{x}$ , $\\hat{y}$ , and $V^d$ (volume of card payments) ")) + theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(0,0.7,0.05)) + annotate("text", x = 0.15, y = 0.67, label =TeX("$V^d$"), size = 10) + annotate("text", x = 0.15, y = 0.56, label =TeX("$\\hat{y}$"), size = 10) + annotate("text", x = 0.15, y = 0.475, label =TeX("$\\hat{x}$"), size = 10)
#
# Plot w.r.t tau (delta = 0) [which ends up simply reversing swapping xhat for yhat but Vd does not change. Hence, no need to put it into the paper.]
ggplot(vol_tau1.df, aes(x=tau1.vec)) + geom_line(aes(y=vd_delta0_tau1.vec), linetype = "solid", size=1.2) + geom_line(aes(y=xhat_delta0_tau1.vec), linetype = "longdash", size=1.2) + geom_line(aes(y=yhat_delta0_tau1.vec),  linetype = "dotdash", size=1.5) + labs(x= TeX("Tax rate on consumer card rewards $\\tau$"), y= TeX("$\\hat{x}$ , $\\hat{y}$ , and $V^d$ (volume of card payments) ")) + theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(0,0.7,0.05)) + annotate("text", x = 0.15, y = 0.67, label =TeX("$V^d$"), size = 10) + annotate("text", x = 0.15, y = 0.56, label =TeX("$\\hat{x}$"), size = 10) + annotate("text", x = 0.15, y = 0.475, label =TeX("$\\hat{y}$"), size = 10)

### Start Figure phi_bar: Draw phi_bar for 2 levels of tau (Figure 3 in paper)
delta1.vec# horiz axis
(tau0.vec = rep(0, length(delta1.vec)))
(tau01.vec = rep(0.1, length(delta1.vec)))
(tau02.vec = rep(0.2, length(delta1.vec)))
#
# phi_bar as function of delta and tau
(phi_bar_delta1_tau0.vec = ((1-tau0.vec)*(beta_p - bph) - (1-delta1.vec)* (beta_c -  bch))/(2*(1-tau0.vec)*(1-delta1.vec)) )
(phi_bar_delta1_tau01.vec = ((1-tau01.vec)*(beta_p - bph) - (1-delta1.vec)* (beta_c -  bch))/(2*(1-tau01.vec)*(1-delta1.vec)) )
(phi_bar_delta1_tau02.vec = ((1-tau02.vec)*(beta_p - bph) - (1-delta1.vec)* (beta_c -  bch))/(2*(1-tau02.vec)*(1-delta1.vec)) )
#
# (1-delta)*phi_bar as function of delta and tau
(phi_bar_x_one_minus_delta1_tau0.vec = (1-delta1.vec) * phi_bar_delta1_tau0.vec)
(phi_bar_x_one_minus_delta1_tau01.vec = (1-delta1.vec) * phi_bar_delta1_tau01.vec)
(phi_bar_x_one_minus_delta1_tau02.vec = (1-delta1.vec) * phi_bar_delta1_tau02.vec)
#
# phi_star = 0
(phi_star.vec = rep(0, length(delta1.vec)))
#
# Make it a data frame
(phi_bar.df = data.frame(delta1.vec, phi_bar_delta1_tau0.vec, phi_bar_delta1_tau01.vec, phi_bar_delta1_tau02.vec, phi_bar_x_one_minus_delta1_tau0.vec, phi_bar_x_one_minus_delta1_tau01.vec, phi_bar_x_one_minus_delta1_tau02.vec, phi_star.vec))
#
# Plotting only for tau=0 and tau=0.2 Figure 3 phi_bar
ggplot(phi_bar.df, aes(x=delta1.vec)) + geom_line(aes(y=phi_bar_delta1_tau0.vec), linetype = "solid", size=1.2) + geom_line(aes(y=phi_bar_delta1_tau02.vec), linetype = "solid", size=1.2) + geom_line(aes(y=phi_bar_x_one_minus_delta1_tau0.vec), linetype = "dotdash", size=1.2, color="red") + geom_line(aes(y=phi_bar_x_one_minus_delta1_tau02.vec), linetype = "dotdash", size=1.2, color="red") + labs(x= TeX("Merchants' rate of tax deduction $\\delta$"), y= TeX("Interchange fee $\\bar{f}$ and effective merchant fee burden  $(1-\\delta)\\cdot\\bar{f}$")) + theme(axis.text.x = element_text(size = 20, color = "black"),  axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(-0.1,0.5,0.05)) + annotate("text", x = 0.43, y = 0.45, label =TeX("$\\bar{f_0}= \\bar{f}(\\delta , \\tau_0)$"), size = 10) + annotate("text", x = 0.35, y = 0.35, label =TeX("$\\bar{f_1} = \\bar{f}(\\delta , \\tau_1)$"), size = 10) + annotate("text", x = 0.35, y = 0.06, label =TeX("$(1-\\delta)\\cdot\\bar{f_1}$"), size = 10, color="red") + geom_segment(aes(x = 0.39, y = 0.35, xend = 0.48, yend = 0.35), arrow = arrow(length = unit(0.5, "cm")), size=1) + annotate("text", x = 0.195, y = 0.06, label =TeX("$(1-\\delta)\\cdot \\bar{f_0}$"), size = 10, color="red") 


### Start Section 5.2, Figure 4: xbar, ybar, x*, y* all as function of f
bch=1
bph=1
beta_c=2.0
beta_p=2.0
# verify that these parameters satisfy Assumption 2
2*beta_p-beta_c < 2*(bch + bph)
2*beta_c-beta_p < 2*(bch + bph)
#
# The optimal xstar and ystar from equation (15) in the paper should be the same:
(xstar.vec = (2*bch + 2*bph + beta_c - 2*beta_p) / (3*beta_c))
#
(ystar.vec = (2*bch + 2*bph - 2*beta_c + beta_p) / (3*beta_p))
#
#Define a vector of f for the given horizontal axis
#(f_seq.vec = seq(-0.5,0.5,0.01))
(f_seq.vec = seq(-1,1,0.01))
length(f_seq.vec)# precision of the horizontal axis
#
# xhat and yhat given delta=tau=0 (just functions fo f)
(xhat_delta0_tau0.vec = (bch-f_seq.vec)/beta_c)
#
(yhat_delta0_tau0.vec = (bph+f_seq.vec)/beta_p)
#
# welfare hat given delta=tau=0
# wc from equation (11) in paper
(wc_delta0_tau0.vec = bch*mp*nc*(yhat_delta0_tau0.vec - xhat_delta0_tau0.vec*(yhat_delta0_tau0.vec-1)) + 0.5*(nc*(2*f_seq.vec*(me+mp*(xhat_delta0_tau0.vec-1)*(yhat_delta0_tau0.vec-1)) + beta_c*(me + mp*(xhat_delta0_tau0.vec^2-1)*(yhat_delta0_tau0.vec-1)))))
# wp from equations (12) in paper
(wp_delta0_tau0.vec = bph*mp*nc*(yhat_delta0_tau0.vec - xhat_delta0_tau0.vec*(yhat_delta0_tau0.vec-1)) + 0.5*(mp*nc*(1-xhat_delta0_tau0.vec)*(2*f_seq.vec*(yhat_delta0_tau0.vec-1) -yhat_delta0_tau0.vec^2*beta_p+beta_p)))
# we from equations (12) in paper
(we_delta0_tau0.vec = nc*me*(beta_e - f_seq.vec))
# total welfare given delta=tau=0
(w_delta0_tau0.vec = wc_delta0_tau0.vec + wp_delta0_tau0.vec + we_delta0_tau0.vec)
#
max(w_delta0_tau0.vec)# highest welfare with interchange fee
which.max(w_delta0_tau0.vec)
f_seq.vec[which.max(w_delta0_tau0.vec)]# which IF max welfare?
#
# welfare* given x* and y* from equation (A.4) in paper
(wstar.vec = bch*mp*nc*(ystar.vec-xstar.vec*(ystar.vec-1)) + bph*mp*nc*(ystar.vec-xstar.vec*(ystar.vec-1)) + me*nc*(beta_c/2 + beta_e) + 0.5*(mp*nc*(xstar.vec^2**beta_c*(ystar.vec-1) + xstar.vec*beta_p*(ystar.vec^2-1) - ystar.vec^2*beta_p - ystar.vec*beta_c + beta_c + beta_p)))
#
# construct a data frame for figure optimal_x_y (Figure 4)
(xy_star.df = data.frame("regulated_IF"=f_seq.vec, "xhat" = xhat_delta0_tau0.vec, "yhat" = yhat_delta0_tau0.vec, "xstar" = xstar.vec, "ystar" = ystar.vec, "w" = w_delta0_tau0.vec, "wstar" = wstar.vec))
#
dim(xy_star.df)
#
# Plot Figure 4 (xhat, yhat, x*, y*, deleting delta* and tau* and deleting \bar) => save unused figure below
ggplot(xy_star.df, aes(x=regulated_IF)) + geom_line(aes(y=xhat), size=1.2) + geom_line(aes(y=yhat), size=1.2) + geom_line(aes(y=xstar.vec), size=1.2, linetype="dotted", color="red") + geom_line(aes(y=ystar.vec), size=1.2, linetype="dotted", color="red")  + theme(axis.text.x = element_text(size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20)) + labs(x= TeX("Regulated (fixed) interchange fee $f_R$"), y= TeX("Comparing $\\hat{x}$ and $\\hat{y}$ with optimal $x^*$ and optimal $y^*$")) + scale_x_continuous(breaks = seq(-1,1,0.25)) + scale_y_continuous(breaks = seq(0,4.3,0.5)) + annotate("text", x = -0.65, y = 0.46, label =TeX("$x^* = y^* = 1/3$"), size = 10, color="red") + annotate("text", x = -0.7, y = 1.15, label =TeX("$\\hat{x}(f_R)$"), size = 10)  + annotate("text", x = 0.75, y = 1.15, label =TeX("$\\hat{y}(f_R)$"), size = 10) + geom_line(aes(y=wstar.vec), size=1.2, linetype="dotted", color="red") + geom_line(aes(y=w), size=1.2) + annotate("text", x = -0.75, y = 4.1, label =TeX("$W^*"), size = 10, color="red")  + geom_line(aes(y=w), size=1.2) + annotate("text", x = 0.75, y = 3.67, label =TeX("$\\hat{W}(f_R)"), size = 10)

### Start section 5.3: Taxation policy and regulation to induce the first-best distribution  [not in paper, just for sake of verification of equation (16) in paper]
bch=1
bph=1
beta_c=2.0
beta_p=2.0
# verify that these parameters satisfy Assumption 2
2*beta_p-beta_c < 2*(bch + bph)
2*beta_c-beta_p < 2*(bch + bph)
#
# The optimal xstar and ystar from equation (15) in the paper should be the same:
(xstar.vec = (2*bch + 2*bph + beta_c - 2*beta_p) / (3*beta_c))
#
(ystar.vec = (2*bch + 2*bph - 2*beta_c + beta_p) / (3*beta_p))
#
#Define a vector of f for the given horizontal axis
#(f_seq.vec = seq(-0.5,0.5,0.01))
(f_seq.vec = seq(-1,1,0.01))
length(f_seq.vec)# precision of the horizontal axis
#
# delta* as function of f_R, equation (16) in paper
(delta_star.vec = -(2*bch-bph-3*f_seq.vec-2*beta_c+beta_p)/(3*f_seq.vec))
# tau* as function of f_R, equation (16) in paper
(tau_star.vec = -(bch-2*bph-3*f_seq.vec-beta_c+2*beta_p)/(3*f_seq.vec))
#
# verify that xhat=xstar and yhat=yhat at delta_star and tau_star
(xhat_verify.vec = (bch-(1-tau_star.vec)*f_seq.vec)/beta_c)
#
(yhat_verify.vec = (bph+(1-delta_star.vec)*f_seq.vec)/beta_p)
#
# Verify equation (17) in paper: tau_star < delta_star
tau_star.vec < delta_star.vec


# end of section 5.3 [not in paper, just for sake of verification of equation (16) in paper]

### Start section 5.4: Taxing card rewards without regulating interchange fees. Figure 5. 
## Figure 5 (top): symmetric case
#
(nc = 1.2) # num of consumers
(mp = 1.2) # num of p-merchants
(me = 0.2*mp)# num of e-merchants
#
bch=1.0
bph=1.0
beta_c=2.0
beta_p=2.0
beta_e=2.0
# verify that these parameters satisfy Assumption 2
2*beta_p-beta_c < 2*(bch + bph)
2*beta_c-beta_p < 2*(bch + bph)
#
# verify that these parameters satisfy Assumption 1a
beta_c > bch
beta_p > bph
# for Assumption 1b see blow because it varies with delta and tau
#
(delta.vec = seq(0,0.5,0.005))# horiz axis
#
(tau_max.vec = rep(NA, length(delta.vec)))
#
(w_max.vec = rep(NA, length(delta.vec)))
#
# loop over i in delta.vec starts
for (i in 1:length(delta.vec)) {
  #
  fbar.fn = function(tau.var){
    ((1-tau.var)*(beta_p-bph) - (1-delta.vec[i])*(beta_c-bch)) / (2*(1-tau.var)*(1-delta.vec[i]))
  }
  #
  xbar.fn = function(tau.var){(bch-(1-tau.var)*fbar.fn(tau.var)) / beta_c}
  #
  ybar.fn = function(tau.var){(bph-(1-delta.vec[i])*fbar.fn(tau.var)) / beta_p}
  #
  w.fn = function(tau.var) {bch*mp*nc*(ybar.fn(tau.var)-xbar.fn(tau.var)*(ybar.fn(tau.var)-1)) +bph*mp*nc*(ybar.fn(tau.var)-xbar.fn(tau.var)*(ybar.fn(tau.var)-1)) +0.5*(nc*(me*(beta_c +2*beta_e) + mp*(xbar.fn(tau.var)^2*beta_c*(ybar.fn(tau.var)-1) +xbar.fn(tau.var)*beta_p*(ybar.fn(tau.var)^2-1) -ybar.fn(tau.var)^2*beta_p -ybar.fn(tau.var)*beta_c +beta_c +beta_p)))}
  #
  tau_max.vec[i] = unlist(optimize(w.fn, interval=c(0,1), maximum = T)[1])
  #
  w_max.vec[i] = unlist(optimize(w.fn, interval=c(0,1), maximum = T)[2])
}# loop over i in delta.vec
#
tau_max.vec
#
w_max.vec
#
# Resulting phi_bar, xbar and ybar
(phi_bar_max.vec = ((1-tau_max.vec)*(beta_p-bph) - (1-delta.vec)*(beta_c-bch)) / (2*(1-tau_max.vec)*(1-delta.vec)) )
# compare with f_bar w/o taxes equation (8) in paper
(phi_bar_no_taxes = 0.5*((beta_p-bph) - (beta_c-bch)))
#
(xbar_max.vec = ((bch+beta_c)*(1-delta.vec) - (beta_p-bph)*(1-tau_max.vec))/(2*beta_c*(1-delta.vec)))
#
(ybar_max.vec = ((bph+beta_p)*(1-tau_max.vec) - (beta_c-bch)*(1-delta.vec))/(2*beta_p*(1-tau_max.vec)))
#
# compare with xstar and ystar
(xstar_max.vec = (2*bch + 2*bph + beta_c -2*beta_p) / (3*beta_c) )
#
(ystar_max.vec = (2*bch + 2*bph -2*beta_c + beta_p) / (3*beta_p))
#
# Verify Assumption 1b
abs((1-delta.vec)*beta_c - (1-tau_max.vec)*beta_p) <= (1-delta.vec)*bch + (1-tau_max.vec)*bph
# making a data.frame
(w_max.df = data.frame(delta.vec, tau_max.vec, w_max.vec, xbar_max.vec, ybar_max.vec, xstar_max.vec, ystar_max.vec, phi_bar_max.vec ))
#
# Plot Figure 5 top (delta_max and tau_max that max w)
ggplot(w_max.df, aes(x=delta.vec)) + geom_line(aes(y=tau_max.vec), size=1.2, color="blue") +geom_line(aes(y=xbar_max.vec), size=1.2, linetype="longdash") + geom_line(aes(y=ybar_max.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xstar_max.vec), size=1.2, linetype="dotted")+ geom_line(aes(y=ystar_max.vec), size=1.2, linetype="dotted") + geom_line(aes(y=phi_bar_max.vec), size=1.2, linetype="dotdash", color="red")+ labs(x= TeX("Assumed merchants' tax deduction rate $\\delta$"), y= TeX("Welfare-maximizing card reward tax rate $\\tau$")) + theme(axis.text.x = element_text(size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(0, 0.65, 0.05)) + annotate("text", x = 0.2, y = 0.58, label =TeX("$\\hat{y}(\\delta, \\tau)$"), size = 10) + annotate("text", x = 0.2, y = 0.16, label =TeX("$\\bar{f}(\\delta, \\tau)$"), size = 10, color="red")+ annotate("text", x = 0.2, y = 0.475, label =TeX("$\\hat{x}(\\delta, \\tau)$"), size = 10) + annotate("text", x = 0.2, y = 0.355, label =TeX("$x^* = y^*$"), size = 10) + annotate("text", x = 0.2, y = 0.03, label =TeX("$\\tau(\\delta)$"), size = 10, color="blue")                                                                   
## Figure 5 (middle): asymmetric case: consumers have higher cash preference than merchants
#
(nc = 1.2) # num of consumers
(mp = 1.2) # num of p-merchants
(me = 0.2*mp)# num of e-merchants
#
bch=1.5 # modification for middle chart
bph=1.0
beta_c=2.0
beta_p=2.0
beta_e=2.0
# verify that these parameters satisfy Assumption 2
2*beta_p-beta_c < 2*(bch + bph)
2*beta_c-beta_p < 2*(bch + bph)
#
# verify that these parameters satisfy Assumption 1a
beta_c > bch
beta_p > bph
# for Assumption 1b see blow because it varies with delta and tau
#
(delta.vec = seq(0,0.5,0.005))# horiz axis
#
(tau_max.vec = rep(NA, length(delta.vec)))
#
(w_max.vec = rep(NA, length(delta.vec)))
#
# loop over i in delta.vec starts
for (i in 1:length(delta.vec)) {
  #
  fbar.fn = function(tau.var){
    ((1-tau.var)*(beta_p-bph) - (1-delta.vec[i])*(beta_c-bch)) / (2*(1-tau.var)*(1-delta.vec[i]))
  }
  #
  xbar.fn = function(tau.var){(bch-(1-tau.var)*fbar.fn(tau.var)) / beta_c}
  #
  ybar.fn = function(tau.var){(bph-(1-delta.vec[i])*fbar.fn(tau.var)) / beta_p}
  #
  w.fn = function(tau.var) {bch*mp*nc*(ybar.fn(tau.var)-xbar.fn(tau.var)*(ybar.fn(tau.var)-1)) +bph*mp*nc*(ybar.fn(tau.var)-xbar.fn(tau.var)*(ybar.fn(tau.var)-1)) +0.5*(nc*(me*(beta_c +2*beta_e) + mp*(xbar.fn(tau.var)^2*beta_c*(ybar.fn(tau.var)-1) +xbar.fn(tau.var)*beta_p*(ybar.fn(tau.var)^2-1) -ybar.fn(tau.var)^2*beta_p -ybar.fn(tau.var)*beta_c +beta_c +beta_p)))}
  #
  tau_max.vec[i] = unlist(optimize(w.fn, interval=c(0,1), maximum = T)[1])
  #
  w_max.vec[i] = unlist(optimize(w.fn, interval=c(0,1), maximum = T)[2])
}# loop over i in delta.vec
#
tau_max.vec
#
w_max.vec
#
# Resulting phi_bar, xbar and ybar
(phi_bar_max.vec = ((1-tau_max.vec)*(beta_p-bph) - (1-delta.vec)*(beta_c-bch)) / (2*(1-tau_max.vec)*(1-delta.vec)) )
# compare with f_bar w/o taxes equation (8) in paper
(phi_bar_no_taxes = 0.5*((beta_p-bph) - (beta_c-bch)))
#
(xbar_max.vec = ((bch+beta_c)*(1-delta.vec) - (beta_p-bph)*(1-tau_max.vec))/(2*beta_c*(1-delta.vec)))
#
(ybar_max.vec = ((bph+beta_p)*(1-tau_max.vec) - (beta_c-bch)*(1-delta.vec))/(2*beta_p*(1-tau_max.vec)))
#
# compare with xstar and ystar
(xstar_max.vec = (2*bch + 2*bph + beta_c -2*beta_p) / (3*beta_c) )
#
(ystar_max.vec = (2*bch + 2*bph -2*beta_c + beta_p) / (3*beta_p))
#
# Verify Assumption 1b
abs((1-delta.vec)*beta_c - (1-tau_max.vec)*beta_p) <= (1-delta.vec)*bch + (1-tau_max.vec)*bph
# making a data.frame
(w_max.df = data.frame(delta.vec, tau_max.vec, w_max.vec, xbar_max.vec, ybar_max.vec, xstar_max.vec, ystar_max.vec, phi_bar_max.vec ))
#
# Plot Figure 5 (middle) (delta_max and tau_max that max w)
ggplot(w_max.df, aes(x=delta.vec)) + geom_line(aes(y=tau_max.vec), size=1.2, color="blue") +geom_line(aes(y=xbar_max.vec), size=1.2, linetype="longdash") + geom_line(aes(y=ybar_max.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xstar_max.vec), size=1.2, linetype="dotted")+ geom_line(aes(y=ystar_max.vec), size=1.2, linetype="dotted") + geom_line(aes(y=phi_bar_max.vec), size=1.2, linetype="dotdash", color="red")+ labs(x= TeX("Assumed merchants' tax deduction rate $\\delta$"), y= TeX("Welfare-maximizing card reward tax rate $\\tau$")) + theme(axis.text.x = element_text(size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(0, 0.65, 0.05)) + annotate("text", x = 0.2, y = 0.565, label =TeX("$\\hat{x}(\\delta, \\tau)$"), size = 10) + annotate("text", x = 0.2, y = 0.31, label =TeX("$\\bar{f}(\\delta, \\tau)$"), size = 10, color="red")+ annotate("text", x = 0.2, y = 0.67, label =TeX("$\\hat{y}(\\delta, \\tau)$"), size = 10) + annotate("text", x = 0.2, y = 0.47, label =TeX("$x^* = y^*$"), size = 10) + annotate("text", x = 0.2, y = 0.05, label =TeX("$\\tau(\\delta)$"), size = 10, color="blue")                                                                          
## Figure 5 (bottom): asymmetric case: consumers have lower cash preference than merchants
#
(nc = 1.2) # num of consumers
(mp = 1.2) # num of p-merchants
(me = 0.2*mp)# num of e-merchants
#
bch=1.0 # 
bph=1.5 # modification for the lower chart
beta_c=2.0
beta_p=2.0
beta_e=2.0
# verify that these parameters satisfy Assumption 2
2*beta_p-beta_c < 2*(bch + bph)
2*beta_c-beta_p < 2*(bch + bph)
#
# verify that these parameters satisfy Assumption 1a
beta_c > bch
beta_p > bph
# for Assumption 1b see blow because it varies with delta and tau
#
(delta.vec = seq(0,0.5,0.005))# horiz axis
#
(tau_max.vec = rep(NA, length(delta.vec)))
#
(w_max.vec = rep(NA, length(delta.vec)))
#
# loop over i in delta.vec starts
for (i in 1:length(delta.vec)) {
  #
  fbar.fn = function(tau.var){
    ((1-tau.var)*(beta_p-bph) - (1-delta.vec[i])*(beta_c-bch)) / (2*(1-tau.var)*(1-delta.vec[i]))
  }
  #
  xbar.fn = function(tau.var){(bch-(1-tau.var)*fbar.fn(tau.var)) / beta_c}
  #
  ybar.fn = function(tau.var){(bph-(1-delta.vec[i])*fbar.fn(tau.var)) / beta_p}
  #
  w.fn = function(tau.var) {bch*mp*nc*(ybar.fn(tau.var)-xbar.fn(tau.var)*(ybar.fn(tau.var)-1)) +bph*mp*nc*(ybar.fn(tau.var)-xbar.fn(tau.var)*(ybar.fn(tau.var)-1)) +0.5*(nc*(me*(beta_c +2*beta_e) + mp*(xbar.fn(tau.var)^2*beta_c*(ybar.fn(tau.var)-1) +xbar.fn(tau.var)*beta_p*(ybar.fn(tau.var)^2-1) -ybar.fn(tau.var)^2*beta_p -ybar.fn(tau.var)*beta_c +beta_c +beta_p)))}
  #
  tau_max.vec[i] = unlist(optimize(w.fn, interval=c(0,1), maximum = T)[1])
  #
  w_max.vec[i] = unlist(optimize(w.fn, interval=c(0,1), maximum = T)[2])
}# loop over i in delta.vec
#
tau_max.vec
#
w_max.vec
#
# Resulting phi_bar, xbar and ybar
(phi_bar_max.vec = ((1-tau_max.vec)*(beta_p-bph) - (1-delta.vec)*(beta_c-bch)) / (2*(1-tau_max.vec)*(1-delta.vec)) )
# compare with f_bar w/o taxes equation (8) in paper
(phi_bar_no_taxes = 0.5*((beta_p-bph) - (beta_c-bch)))
#
(xbar_max.vec = ((bch+beta_c)*(1-delta.vec) - (beta_p-bph)*(1-tau_max.vec))/(2*beta_c*(1-delta.vec)))
#
(ybar_max.vec = ((bph+beta_p)*(1-tau_max.vec) - (beta_c-bch)*(1-delta.vec))/(2*beta_p*(1-tau_max.vec)))
#
# compare with xstar and ystar
(xstar_max.vec = (2*bch + 2*bph + beta_c -2*beta_p) / (3*beta_c) )
#
(ystar_max.vec = (2*bch + 2*bph -2*beta_c + beta_p) / (3*beta_p))
#
# Verify Assumption 1b
abs((1-delta.vec)*beta_c - (1-tau_max.vec)*beta_p) <= (1-delta.vec)*bch + (1-tau_max.vec)*bph
# making a data.frame
(w_max.df = data.frame(delta.vec, tau_max.vec, w_max.vec, xbar_max.vec, ybar_max.vec, xstar_max.vec, ystar_max.vec, phi_bar_max.vec ))
#
# Plot Figure 5 (bottom) (delta_max and tau_max that max w)
ggplot(w_max.df, aes(x=delta.vec)) + geom_line(aes(y=tau_max.vec), size=1.2, color="blue") +geom_line(aes(y=xbar_max.vec), size=1.2, linetype="longdash") + geom_line(aes(y=ybar_max.vec), linetype="longdash", size=1.2) + geom_line(aes(y=xstar_max.vec), size=1.2, linetype="dotted")+ geom_line(aes(y=ystar_max.vec), size=1.2, linetype="dotted") + geom_line(aes(y=phi_bar_max.vec), size=1.2, linetype="dotdash", color="red")+ labs(x= TeX("Assumed merchants' tax deduction rate $\\delta$"), y= TeX("Welfare-maximizing card reward tax rate $\\tau$")) + theme(axis.text.x = element_text(size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(-0.25, 0.75, 0.05)) + annotate("text", x = 0.2, y = 0.55, label =TeX("$\\hat{x}(\\delta, \\tau)$"), size = 10) + annotate("text", x = 0.2, y = -0.14, label =TeX("$\\bar{f}(\\delta, \\tau)$"), size = 10, color="red")+ annotate("text", x = 0.2, y = 0.73, label =TeX("$\\hat{y}(\\delta, \\tau)$"), size = 10) + annotate("text", x = 0.2, y = 0.46, label =TeX("$x^* = y^*$"), size = 10) + annotate("text", x = 0.2, y = 0.05, label =TeX("$\\tau(\\delta)$"), size = 10, color="blue")                                                                                            
### Start Figure tau versus delta => Removed from paper
# # check parameter values
# beta_c
# bch
# beta_c-bch
# beta_p
# bph
# beta_p-bph
# #
# (tau_opt.vec = delta1.vec*(beta_p-bph)/ ((beta_c-bch)*(1-delta1.vec) + (beta_p-bph)*delta1.vec))
# #
# # raise beta_p=3, keep beta_c=2
# (beta_p3=3)
# (tau_opt_p3.vec = delta1.vec*(beta_p3-bph)/ ((beta_c-bch)*(1-delta1.vec) + (beta_p3 -bph)*delta1.vec))
# #
# # Verify Assumption 1a
# beta_c > bch
# beta_p3 > bph
# # Verify Assumption 1b
# abs((1-delta1.vec)*beta_c - (1-tau_opt_p3.vec)*beta_p3) <= (1-delta1.vec)*bch + (1-tau_opt_p3.vec)*bph
# #
# 
# # Instead raise beta_c=3 keeping beta_p=2
# (beta_c3=3)
# (tau_opt_c3.vec = delta1.vec*(beta_p-bph)/ ((beta_c3-bch)*(1-delta1.vec) + (beta_p -bph)*delta1.vec))
# #
# #  Verify Assumption 1a
# beta_c3 > bch
# beta_p > bph
# # Verify Assumption 1b
# abs((1-delta1.vec)*beta_c3 - (1-tau_opt_c3.vec)*beta_p) <= (1-delta1.vec)*bch + (1-tau_opt_c3.vec)*bph
# #
# 
# # making a data frame
# (tau_opt.df = data.frame(delta1.vec, tau_opt.vec, tau_opt_p3.vec, tau_opt_c3.vec))
# 
# # Plotting high beta_p=3, high_beta_c=3 and benchmark beta_p=beta_c=2. => Removed from paper
# ggplot(tau_opt.df, aes(x=delta1.vec)) + geom_line(aes(y=tau_opt.vec), linetype = "solid", size=1.2) + geom_line(aes(y= tau_opt_p3.vec), linetype = "dashed", size=1.2, color="red") + geom_line(aes(y= tau_opt_c3.vec), linetype = "dotdash", size=1.2, color="blue") + labs(x= TeX("Merchants' rate of tax deduction $\\delta$"), y= TeX("Optimal tax on card rewards  $\\tau^*$")) + theme(axis.text.x = element_text(size = 20, color = "black"),  axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0,0.5,0.05)) + scale_y_continuous(breaks = seq(-0.1,0.65,0.05)) + annotate("text", x = 0.4, y = 0.62, label =TeX("High $\\beta_p$ case"), size = 10, color="red") + annotate("text", x = 0.4, y = 0.19, label =TeX("High $\\beta_c$ case"), size = 10, color="blue") + annotate("text", x = 0.4, y = 0.33, label =TeX("Benchmark case"), size = 10, color="black") 

### Start Figure 4: optimal_tax: for every given delta, plot tau_star and resulting phi_bar using tax_intfee_2022_12_25.dfw. It also shows why phi_bar is inconsistent with tau_star and delta_star
# bch=1
# bph=1
# beta_c=2.0
# beta_p=2.0
# # verify that these parameters satisfy Assumption 2
# 2*beta_p-beta_c < 2*(bch + bph)
# 2*beta_c-beta_p < 2*(bch + bph)
# #
# # The optimal xstar and ystar from equation (14) in the paper should be the same:
# (xstar.vec = (2*bch + 2*bph + beta_c - 2*beta_p) / (3*beta_c))
# #
# (ystar.vec = (2*bch + 2*bph - 2*beta_c + beta_p) / (3*beta_p))
# #
# # simulate fstar_x and fstar_y eq (A.9) in paper
# (fstar_x = (bch -2*bph -beta_c +2*beta_p)/3)
# #
# (fstar_y = (2*bch -bph -2*beta_c +beta_p)/3)
# #
# # Verify again that fstar_x and _y support xstar and ystar via xhat and yhat
# (xhat_test = (bch - fstar_x)/beta_c)
# (yhat_test = (bph + fstar_y)/beta_p)
# #
# #Define a vector of f for the given horizontal axis
# #(f_seq.vec = seq(-0.5,0.5,0.01))
# (f_seq.vec = seq(0.5,1,0.005))
# length(f_seq.vec)# precision of the horizontal axis
# # delta_star and tau_star from equation (15) in paper
# (delta_star.vec = (f_seq.vec -fstar_y)/f_seq.vec)
# #
# (tau_star.vec = (f_seq.vec -fstar_x)/f_seq.vec)
# # Verify again that fstar_x and _y support xstar and ystar via xhat and yhat
# (xhat_test2 = (bch - (1-tau_star.vec)*f_seq.vec)/beta_c)
# (yhat_test2 = (bph + (1-delta_star.vec)*f_seq.vec)/beta_p)
# #
# #verifying equation (15) in paper
# (delta_star.vec = -(2*bch - bph -3*f_seq.vec -2*beta_c + beta_p) / (3*f_seq.vec))
# #
# (tau_star.vec = -(bch - 2*bph -3*f_seq.vec -beta_c + 2*beta_p) / (3*f_seq.vec))
# #
# # Verify that xbar and yhat from (B.1) and (B.2) in paper support (equal) xstar and ystar
# (xbar_test3.vec = (bch - (1-tau_star.vec)*f_seq.vec)/beta_c)
# #
# (ybar_test3.vec = (bph +(1-delta_star.vec)*f_seq.vec)/beta_p)
# #
# # consumer burden (same as fstar_x.vec and fstar_y.vec)
# (burden_c.vec = (1-tau_star.vec)*f_seq.vec)
# # merchant burden
# (burden_m.vec = (1-delta_star.vec)*f_seq.vec)
# #
# # f_bar.vec from equation (7) in paper under the computed delta_star.vec & tau_star.vec
# (f_bar_delta_tau_star.vec = ((1-tau_star.vec)*(beta_p - bph) - (1-delta_star.vec)* (beta_c -  bch))/(2*(1-tau_star.vec)*(1-delta_star.vec)) )
# #
# # construct a data frame for figure optimal_tax (Figure 4)
# (tax_star.df = data.frame("Regulated_IF"=f_seq.vec, "Delta_star"=delta_star.vec, "Tau_star" = tau_star.vec, "xstar" =xstar.vec, "ystar" =ystar.vec, xhat_test, yhat_test, "IF_bar" =f_bar_delta_tau_star.vec))
# dim(tax_star.df)
# #
# # Plot Figure 4 (xhat, yhat, x*, y*, deleting delta* and tau* and deleting \bar) => save unused figure below
# ggplot(tax_star.df, aes(x=Regulated_IF)) + geom_line(aes(y=Delta_star), size=1.2) + geom_line(aes(y=Tau_star), size=1.2) + geom_line(aes(y=IF_bar), linetype="longdash", color="red", size=1.2) + geom_line(aes(y=Regulated_IF), linetype = "dotted", size=1.2, color="red") + labs(x= TeX("Regulated (fixed) interchange fee $f_R"), y= TeX("Optimal deduction $\\delta^*$, tax $\\tau^*$, and card company's $\\bar{f}(\\delta^*,\\tau^*)$")) + theme(axis.text.x = element_text(size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), text = element_text(size = 20))+ scale_x_continuous(breaks = seq(0.5,1,0.05)) + scale_y_continuous(breaks = seq(-3,2,0.5)) + annotate("text", x = 0.55, y = -2.05, label =TeX("$\\bar{f}(\\delta^*,\\tau^*)$"), size = 10, color="red") + annotate("text", x = 0.55, y = 0.17, label =TeX("$\\tau^*(f_R)$"), size = 10) + annotate("text", x = 0.55, y = 1.38, label =TeX("$\\delta^*(f_R)$"), size = 10) + annotate("text", x = 0.75, y = 0.9, label =TeX("$45$-degree line"), size = 8, color="red")

