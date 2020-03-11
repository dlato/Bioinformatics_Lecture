# graphs to show info about sequencing data

##########################
library(zoo)#to deal with dates in odd formats
library(ggplot2)

theme_set(theme_bw()
          + theme(#axis.text.x=element_blank(),
                  #axis.ticks.x=element_blank(),
                  #axis.text.y=element_blank(),
                  axis.title.x=element_text(color = "#213862"),
                  axis.title.y=element_text(color = "#213862"),
                  #axis.ticks.y=element_blank(),
                  plot.title = element_text(color = "#231862",hjust = 0.5), 
                  text = element_text(size=24))
          + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "#213862"))
)

library(qrcode)
##########################
#create QR code for presentation
pdf("./figs/qr_code.pdf")
qrcode_gen('https://github.com/dlato/Bioinformatics_Lecture')
dev.off()


# cost of sequencing over the years
#load in data
cost_dat <- read.csv("sequencing_cost_per_genome.csv")
#convert dates to dates R understands
cost_dat$Date <- as.Date(as.yearmon(cost_dat$Date, "%b-%Y"))
#rescaling cost by 1000
cost_dat$CostperGenome <- cost_dat$CostperGenome/1000

options(scipen=10000)
seq_cost <- (ggplot(data=cost_dat, aes(x=Date, y=CostperGenome, group=1))
  + geom_line(color="#087E8B", size=4)
  #  geom_line(color="#FE5F55", size=4)
  #+ geom_point(pch=".", color="#1A936F")
  #  geom_point(pch=".", color="#FE5F55")
  #  geom_line(color="#FE5F55", size=2)
  + geom_line(color="#087E8B", size=2)
  #  geom_point(pch=".", color="#CBE896")
  #  geom_point(pch=".", color="#FE5F55")
  #+ ggtitle(expression(paste(underline("Sequencing Cost per Human Genome"))))
  + ggtitle("Sequencing Cost per Human Genome")
  #+ ggtitle("")
  + labs(x = "", y = "Cost ($1,000 USD)")
  + scale_y_continuous(trans='log10',labels = function(x) ifelse(x ==0, "0", x),breaks=c(0.1,1,10,100,1000,10000,100000))
  + scale_x_date(breaks = as.Date(c("1985-01-01","1990-01-01","2001-09-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2019-01-01")),
                 minor_breaks = function(x) seq.Date(from = min(x), 
                                                     to = max(x), 
                                                     by = "2 years"),date_labels =  "%Y")
  
)
pdf(width = 9, height = 7, "seq_cost_graph.pdf")
seq_cost
dev.off()


########################################
# number of sequences over the years
#all data from emble
#load in data
seq_dat <- read.table("embl.data", header = TRUE)
#convert dates to dates R understands
seq_dat$date <- as.Date(with(seq_dat, paste(year, mnth, day,sep="-")), "%Y-%m-%d")

#calculation for how much GB of data this would be
max_bp <- max(seq_dat$bp)
# each bp = 2 bytes of data
total_bytes <- max_bp * 2
#make this number human redable (in GB)
total_GB <- total_bytes / 1^9
total_GB
#total number of laptops this would be if a laptop has 240GB storage
total_num_laptops <- total_GB / 512
total_num_laptops
#about 22 trillion laptops

#rescaling cost by 1 billion
seq_dat$bp <- seq_dat$bp/1000000000000

options(scipen=10000)
########### NEED TO FIX THE X AXIS TICK LABELS
seq_num <- (ggplot(data=seq_dat, aes(x=date, y=bp, group=1))
             + geom_line(color="#087E8B", size=4)
             #+ geom_point(pch=".", color="#1A936F")
             + geom_line(color="#087E8B", size=2)
             #+ ggtitle(expression(paste(underline("Sequencing Cost per Human Genome"))))
             + ggtitle("EMBL Sequence Data")
             #+ ggtitle("")
             + labs(x = "", y = "Total Base Pairs (trillions)")
             #+ scale_y_continuous(trans='log10',labels = function(x) ifelse(x ==0, "0", x),breaks=c(0.1,1,10,100,1000,10000,100000))
             #+ scale_x_discrete(breaks = c(2019-04-01))
             #+ scale_x_date(breaks = function(x) seq.Date(from = min(x), 
            #                                              to = max(x), 
             #                                             by = "6 years"),
             + scale_x_date(breaks = as.Date(c("1985-01-01","1990-01-01","1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2019-01-01")),
                            minor_breaks = function(x) seq.Date(from = min(x), 
                                                                to = max(x), 
                                                                by = "2 years"),date_labels =  "%Y")
            
            
)
pdf(width = 9, height = 7, "seq_num_graph.pdf")
seq_num
dev.off()

