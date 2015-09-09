    na1 <- subset(tmp, tmp@coords[,1]>bbox[1,1] & tmp@coords[,1]<bbox[1,2] 
        & tmp@coords[,2]>bbox[2,1] & tmp@coords[,2]<bbox[2,2])
    na2 <- subset(tmp2, tmp2@coords[,1]>bbox[1,1] & tmp2@coords[,1]<bbox[1,2] 
        & tmp2@coords[,2]>bbox[2,1] & tmp2@coords[,2]<bbox[2,2])
    na3 <- subset(tmp3, tmp3@coords[,1]>bbox[1,1] & tmp3@coords[,1]<bbox[1,2] 
        & tmp3@coords[,2]>bbox[2,1] & tmp3@coords[,2]<bbox[2,2])
    png(file = paste0("./pics/", tname, "_soil.png"), width = 730, height = 480, units = "px")
    spplot(na1["BDRICM"], xlim = na2@bbox[1,], ylim = na2@bbox[2,], colorkey = list(
    	right = list( 
    		fun = draw.colorkey, 
    		args = list(
    			key = list(
    				at = seq(0, 200, 20), # colour breaks
    				col = bpy.colors(11), # colours
    				labels = list(
    					at = c(0, 40, 80, 120, 160, 200), 
    					labels = c("0", "40", "80", "120", "160", "200"),
                        cex = 2
    				)
    			)
    		)
    	)
    ))
    plot(country, col="darkgrey")
    
    dev.off()
    
    png(file = paste0("./pics/", tname, "_well<200.png"), width = 730, height = 480, units = "px")
    spplot(na2["BDRICM"], xlim = na1@bbox[1,], ylim = na1@bbox[2,], colorkey = list(
    	right = list( 
    		fun = draw.colorkey, 
    		args = list(
    			key = list(
    				at = seq(0, 200, 20), # colour breaks
    				col = bpy.colors(11), # colours
    				labels = list(
    					at = c(0, 40, 80, 120, 160, 200), 
    					labels = c("0", "40", "80", "120", "160", "200"),
                        cex = 2
    				)
    			)
    		)
    	)
    ))
    dev.off()
  
    png(file = paste0("./pics/", tname, "_well>200.png"), width = 730, height = 480, units = "px" )
    spplot(na3["BDRICM"], xlim = na1@bbox[1,], ylim = na1@bbox[2,],  colorkey = list(
    	right = list( 
    		fun = draw.colorkey, 
    		args = list(
    			key = list(
    				at = seq(200, 400, 20), # colour breaks
    				col = bpy.colors(11), # colours
    				labels = list(
    					at = c(200, 250, 300, 350, 400), 
    					labels = c("200", "250", "300", "350", ">400"),
                        cex = 2
    				)
    			)
    		)
    	)
    ))
    dev.off()
