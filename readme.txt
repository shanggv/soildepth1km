order of execution:
1, rw_*.r
2, rw_*.r in \SoilGrids1km\profiles
3, wsp_DB.R in \SoilGrids1km\profiles
4, part of samples_sprofs.R in \SoilGrids1km\profiles
5, preparePoints.R
6, getartpoints.r
7, getoverlay.r
8, getcolinear.r
9, getcovariates.r
10, getsubpoints.r
11, ---feature_space.r
12, fit.n.r £¨fit_models.r£©
13£¬fit.n.region.r£¨fit_models.region.r£©
14£¬cv.n.r (cross.validation.r, cv.plot.r, cv_*.png)
15, cv.region.n.r£¨cv.region.r£¬BDRICM_val_0*.png£¬ fit_0all.png£©
16£¬cv.region.n2.r£¨cv.region2.r£¬ CCsim*.tiff£¬USdis.tiff£¬USsimaccbox.tiff£©
17, pre.region.r
17£¬com.celln.n.r£¨com.celln.r£©
18, val.en.r (subsnewr2.png)
19, pred.n.r£¨prediction.r£©
20, ---compare.r
21, ---compare.r2
22, ---ziptiles.r
23, plot.points.r£¨plot.points.spplot.r£¬  EDA*.png, all*.png,na*.png, p.*.png£¬ Hist*.tiff£©
24£¬plot.model.r (m_*.png, vs*.png)