#!/usr/bin/perl -w

#BSD 3-Clause License
#
#Copyright (c) 2021, dabiaCBM
#All rights reserved.
#
#Redistribution and use in source and binary forms, with or without
#modification, are permitted provided that the following conditions are met:
#
#1. Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
#2. Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
#3. Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
#FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
#OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


$ch1='FL2.A'; #Data channel 1
$ch2='FL7.A'; #Data channel 2

$scale=0; #Scale flowcytometry data according to FCS file parameters


#Parameters for gating using parabolic shapes
$filter1=0; #Use parabolic gating over FSC.H and SSC.A channels
$filter2=0;  #Use parabolic gating over FL2.A and FL7.A channels
$scalef1=2.0; #FSC.H and SSC.A parabolic gating scale factor
$scalef2=4.0; # FL2.A and FL7.A parabolic gating scale factor

#Parameters for gating using K-clustering 
$clusterg1=1; #First gating using K clustering
$clusterg2=1; #Second gating using K clustering
$clg1_ch1='FSC.A'; #Channel 1 for first K-clust gating
$clg1_ch2='SSC.A'; #Channel 2 for first K-clust gating
$clg1_ch1_max=200000;  #Max values  for first K-clust gating  #FSC:200000, SSC:20000
$clg1_ch2_max=20000;
$clg1_nc=2; #Max number of clusters on first K-clust gating
$clg1_level=0.75; #Percentage data for first K-clust gating
$clg1_min=20; # Min value for first K-clust gating
$clg1_max=200000; # Max value for first K-clust gating
$clg1_th=0.9999; #Discard points over this threshold for first K-clust gating
$clg1_bic_th=0.3; #BIC improvement threshold for choosing two clusters instead of one in first K-clust gating
$clg2_ch1=$ch2; #Channel 1 for second K-clust gating
$clg2_ch2=$ch1; #Channel 2 for second K-clust gating
$clg2_ch1_max=50000;    #Max values for second K-clust gating  #FL2:50000, FL7:50000
$clg2_ch2_max=50000;
$clg2_nc=1;  #Max number of clusters on second K-clust gating
$clg2_level=0.90;  #Percentage data for second K-clust gating
$clg2_min=0;  # Min value for second K-clust gating
#$clg2_max=200000;
$clg2_th=0.98; #Discard points over this threshold for second K-clust gating
$clg2_bic_th=0.5;  #BIC improvement threshold for choosing two clusters instead of one in second K-clust gating
$clg2_log=1; #Log transform data before second K-clust gating

#Parameters for MMSkew gaussian adjust
$type='mvn'; #mvn: multivariate normal, msn: multivariate skew normal, mvt: multivariate t-student, mst: multivariate skew t-student
$nmix=1; #Number of gaussians
$lowfilter=1; #Discard values below this threshold (in log scale)

#Report parameters
$plotnrows=6; #Number of samples per page in pdf

if ($clg2_log == 1){
	$clg2_ch1="${clg2_ch1}_log";
	$clg2_ch2="${clg2_ch2}_log";
}

#Set number of columns in output plots
$ncl=0;
if ($clusterg1){
	$ncl+=$clg1_nc;
}
if ($clusterg2){
	$ncl+=$clg2_nc;
}
$ncol=4;
if ($ncl>2){
	$ncol=$ncl;
}
$ncol++;#Add one column for mskew plot

#Mmskew accesory parameters
$first=1;
$header='Sample';
for $m(1..(2*$nmix)){
	$header.="\tM$m";
}
$sigmas='c(';
for $s(1..(4*$nmix)){
	if ( $s % 3 != 0){
		$header.="\tS$s";
		if ($first == 1){
			$sigmas.= $s;
			$first=0;
		}else{
			$sigmas.= ",$s";
		}
	}
}
$sigmas.=')';
if ($type eq 'msn'){
	for $d(1..(2*$nmix)){
		$header.="\tD$d";
	}
}
if($clusterg1==1 and $clg1_nc==2){
	$header.="\t".join("\t",qw(2cl_BIC 2cl_ICL 2cl_logLike));
}
if($clusterg2==1){
	$header.="\t".join("\t",qw(g2_BIC g2_ICL g2_logLike));
}

$rname='';
$rname.="g$nmix";
$rname.="-$type";


open (RSCR,'>extract_and_class_FCDat.R');
#print RSCR "library(flowCore)\nlibrary(flowStats)\nlibrary(flowWorkspace)\nlibrary(ggcyto)\n";
print RSCR "library(flowCore)\nlibrary(flowStats)\n#library(flowWorkspace)\n#library(ggcyto)\n";
print RSCR 'library(EMMIXskew)',"\n";
print RSCR 'library(imager)',"\n";
if ($clusterg1 == 1 or $clusterg2 == 1 ){
	print RSCR "library(flowClust)\n";
	#print RSCR "pdf(file='flowclust.pdf',pointsize=6,width=8.3,height=11.7)\n";
	#print RSCR "png(file='flowclust.png',pointsize=25,width=2480,height=3508)\n";
	#print RSCR "par(mfrow = c(6,4))\n";
}


print RSCR "sink('clg1-clg2-log-noneg.csv')\n";
print RSCR "cat('$header','\\n')\n";

#library(flowClust)
#	flowdata_BA_001_sc<-read.FCS('Jurkat_Spike_BA_001_028.fcs',transformation='scale',alter.names=TRUE)
#	flowdata_BA_001_sc_comp<-compensate(flowdata_BA_001_sc,spillover(flowdata_BA_001_sc)[[1]])
#	flowdata_BA_001_sc_comp_log<-transform(flowdata_BA_001_sc_comp,`FL2.A_log`=log(`FL2.A`),`FL7.A_log`=log(`FL7.A`),`SSC.A_log`=log(`SSC.A`),`FSC.A_log`=log(`FSC.A`))
#	ggplot(flowdata_BA_001_sc_comp_log, aes(x=FSC.A_log,y=SSC.A_log))+geom_bin2d(bins = 500) +scale_fill_continuous(type = "viridis") +theme_bw() +xlim(0,175000)+ylim(0,10000)
#n2f<-norm2Filter(filterId="myNorm2Filter", x=c("FSC.A", "SSC.A"),scale.factor=2)
#flowdata_filt<-Subset(flowdata,filter(flowdata,n2f))
#sel_FSC.A<-exprs(flowdata$FSC.A[flowdata$FSC.A>20 & flowdata$FSC.A<200000])
#sel_SSC.A<-exprs(flowdata$SSC.A[flowdata$SSC.A>20 & flowdata$SSC.A<200000])
#hist_FSC.A<-hist(sel_FSC.A,breaks=100)
#hist_SSC.A<-hist(sel_SSC.A,breaks=100)
#FSC.A.max<-head(hist_FSC.A$breaks[cumsum(hist_FSC.A$counts)/ nrow(sel_FSC.A) > 0.98],1)
#SSC.A.max<-head(hist_SSC.A$breaks[cumsum(hist_SSC.A$counts)/ nrow(sel_SSC.A) > 0.98],1)
##res1_mult_SA_FA <-flowClust(flowdata,varNames = c("FSC.A", "SSC.A"),K = 1:4, B = 100,level=0.9,min=c(20,20),max=c(FSC.A.max,SSC.A.max))
## pl_title<-paste('AB_Tube_001_148 BIC:',round(criterion(res1_mult_FSC.A_SSC.A_1,'BIC')),'Prop:',toString(round(slot(res1_mult_FSC.A_SSC.A_2,'w'),2)), sep = " ")
##plot(res1_mult_SA_FA[[1]],level=0.9,z.cutoff=0,data=flowdata)
#s2filter <- tmixFilter("s2filter", c("FSC.A", "SSC.A"), K = 4, B = 100,level=0.9,min=c(20,20),max=c(FSC.A.max,SSC.A.max)) 
#res1_mult_SA_FA<-filter(flowdata,s2filter)
#flowdata_split<-split(flowdata,res1_mult_SA_FA_2,population = list(sc1 = 1, sc2 = 2,sc3=3,sc4=4))
#ggplot(data=flowdata_split$sc4, aes(x=FSC.A,y=SSC.A))+geom_bin2d(bins = 500) +scale_fill_continuous(type = "viridis") +theme_bw()
#ggplot(data=flowdata, aes(x=FSC.A,y=SSC.A))+geom_bin2d(bins = 500) +scale_fill_continuous(type = "viridis") +theme_bw() +xlim(0,200000)+ylim(0,20000)
#
if ($filter1 == 1){
	print RSCR "n2f<-norm2Filter(filterId='myNorm2Filter', x=c('FSC.A', 'SSC.A'),scale.factor=$scalef1)\n";
}
if ($filter2 == 1){
	#print RSCR "n2f2<-norm2Filter(filterId='myNorm2Filter', x=c('FL2.A', 'FL7.A'),scale.factor=2)\n";
	print RSCR "n2f2<-norm2Filter(filterId='myNorm2Filter2', x=c('${ch1}_log', '{$ch2}_log'),scale.factor=$scalef2)\n";
}
$nout=0;
#$nplots=-1;
$first=1;
$pngfiles='';
$nsamp=0;
#$firstsample=1;
print RSCR "write('Processing samples',stderr())\n";
foreach $fcs(@ARGV){
	$fcs=~/(.*).fcs/;
	$sample=$1;
	if ($filter1==1){
		$csv="$sample.gated.log.csv";
	}else{
		$csv="$sample.log.csv";
	}
	if ( -e $csv ){
		print STDERR "Warning: Skipping sample $fcs, already processed\n";
		next;
	}
	$csv=~s/\s+/-/g;
	print RSCR "write('$fcs',stderr())\n";
	if ($scale == 1){
		print RSCR "flowdata<-read.FCS('$fcs',transformation='scale',alter.names=TRUE)\n";
		#print RSCR "flowdata<-read.FCS('$fcs',transformation='scale',alter.names=TRUE,min.limit=1)\n";
		#print RSCR "flowdata_sc_comp<-compensate(flowdata_sc,spillover(flowdata_sc)[[1]])\n";
	}else{
		#print RSCR "flowdata<-read.FCS('$fcs',alter.names=TRUE,min.limit=1)\n";
		print RSCR "flowdata<-read.FCS('$fcs',alter.names=TRUE)\n";
	}
	if ($filter1 == 1){
		print RSCR "flowdata<-Subset(flowdata,filter(flowdata,n2f))\n";
	}
	if ($clusterg1){
		
		#$nplots++;
		$nsamp++;
		#if ($nplots%(24/$ncl) == 0){

		if ($nsamp > $plotnrows or $first==1){
			$nsamp=1;
			$nout++;
			if ($first != 1){
				print RSCR "plotlog<-dev.off()\n";
			}else{
				$first=0;
			}
			$created = 1;
			while ($created == 1){
				$pngfile = "flowclust-$nout.png";
				$pngfiles.=" flowclust-$nout.png";
				if (-e $pngfile){
					$nout++;
				}else{
					$created=0;
				}
			}
			print RSCR "png(file='flowclust-$nout.png',pointsize=25,width=2480,height=3508)\n";
			print RSCR "par(mfrow = c($plotnrows,$ncol))\n";
		}
		print RSCR "sel_$clg1_ch1<-exprs(flowdata\$${clg1_ch1}\[flowdata\$$clg1_ch1>$clg1_min & flowdata\$$clg1_ch1<$clg1_max\])\n";
		print RSCR "sel_$clg1_ch2<-exprs(flowdata\$${clg1_ch2}\[flowdata\$$clg1_ch2>$clg1_min & flowdata\$$clg1_ch2<$clg1_max\])\n";
		print RSCR "hist_$clg1_ch1<-hist(sel_$clg1_ch1,breaks=100,plot = FALSE)\n";
		print RSCR "hist_$clg1_ch2<-hist(sel_$clg1_ch2,breaks=100,plot = FALSE)\n";
		print RSCR "${clg1_ch1}.max<-min(head(hist_${clg1_ch1}\$breaks[cumsum(hist_${clg1_ch1}\$counts)/ nrow(sel_$clg1_ch1) > $clg1_th],1),$clg1_ch1_max)\n";
		print RSCR "${clg1_ch2}.max<-min(head(hist_${clg1_ch2}\$breaks[cumsum(hist_${clg1_ch2}\$counts)/ nrow(sel_$clg1_ch2) > $clg1_th],1),$clg1_ch2_max)\n";

		for $nc(1..$clg1_nc){
			#print RSCR "clfilter$nc <- tmixFilter('clfilter$nc', c('$clg1_ch1', '$clg1_ch2'), K = $nc, randomStart=10, B = 100,level=$clg1_level,min=c($clg1_min,$clg1_min),max=c(${clg1_ch1}.max,${clg1_ch2}.max),nu.est=1)\n";
			print RSCR "clfilter$nc <- tmixFilter('clfilter$nc', c('$clg1_ch1', '$clg1_ch2'), K = $nc, randomStart=10, B = 100,level=$clg1_level,min=c($clg1_min,$clg1_min),max=c(${clg1_ch1}.max,${clg1_ch2}.max))\n";
			$resname="res_mult_${clg1_ch1}_${clg1_ch2}_$nc";
			print RSCR "$resname<-filter(flowdata,clfilter$nc)\n";
			$pl_title="pl_title<-paste('$sample BIC:',round(criterion($resname,'BIC')),'\\nProp:',toString(round(slot($resname,'w'),2))";
			if ($nc > 1){
				print RSCR "bicratio<-(bic1/criterion($resname,'BIC')-1)*100\n";
				$pl_title.=",'\\nBIC ratio:',round(bicratio,2), sep = ' ')\n";
			}else{
				$pl_title.=", sep = ' ')\n";
				print RSCR "bic1<-criterion($resname,'BIC')\n";
			}
			print RSCR $pl_title;
			print RSCR "plotlog<-plot($resname,level=$clg1_level,z.cutoff=0,data=flowdata,xlim=c(0,${clg1_ch1}.max*1.1),ylim=c(0,${clg1_ch2}.max*1.1),main=pl_title)\n";	
			#print RSCR "flowdata_split_${clg1_ch1}_${clg1_ch2}_$nc<-split(flowdata,res1_mult_${clg1_ch1}_${clg1_ch2}_$nc,population = list(sc1 = 1\n";
			#for $nc2(2..$nc){
			#	print RSCR ",sc$nc2=$nc2";
			#}
			#print RSCR "))\n";
			#print RSCR "ggplot(data=flowdata_split$sc4, aes(x=FSC.A,y=SSC.A))+geom_bin2d(bins = 500) +scale_fill_continuous(type = "viridis") +theme_bw()
		}
		$resname1="res_mult_${clg1_ch1}_${clg1_ch2}_1";
		if ($clg1_nc > 1){
			$resname2="res_mult_${clg1_ch1}_${clg1_ch2}_2";
			print RSCR "if (bicratio>$clg1_bic_th){\n";
			print RSCR "\tcl<-which.max(slot($resname2,'w'))\n";
			print RSCR "\tflowdata<-split(flowdata,$resname2,population = list(sc1 = cl))[[1]]\n";
			print RSCR "\}else{\n";
			print RSCR "\tflowdata<-split(flowdata,$resname1)[[1]]\n";
			print RSCR "}\n";

		}else{
			print RSCR "flowdata<-split(flowdata,$resname1)[[1]]\n";
		}
	}
	print RSCR "flowdata_comp<-compensate(flowdata,spillover(flowdata)[[1]])\n";

	print RSCR "min_$ch1<-min(exprs(flowdata_comp\$$ch1))\n";
	print RSCR "min_$ch2<-min(exprs(flowdata_comp\$$ch2))\n";
	print RSCR "if (min_$ch1<=0){\n";
	print RSCR "\tflowdata_comp<-transform(flowdata_comp,`${ch1}`=`$ch1`-min_$ch1+1)\n";
	print RSCR "}\n";
	print RSCR "if (min_$ch2<=0){\n";
	print RSCR "\tflowdata_comp<-transform(flowdata_comp,`${ch2}`=`$ch2`-min_$ch2+1)\n";
	print RSCR "}\n";

	#print RSCR "flowdata<-transform(flowdata_comp[(flowdata_comp\$$ch1>1 & flowdata_comp\$$ch2>1),],`${ch1}_log`=log(`$ch1`),`${ch2}_log`=log(`$ch2`))\n";
	#print RSCR "flowdata<-transform(flowdata_comp,`${ch1}_log`=log(`$ch1`),`${ch2}_log`=log(`$ch2`))\n";
	print RSCR "flowdata<-transform(flowdata_comp,`${ch1}_log`=log(`$ch1`),`${ch2}_log`=log(`$ch2`))\n";



	if ($clusterg2){
		#$nplots++;
		#if ($nplots%(24/$ncl) == 0){
		if ($clusterg1==0){
			$nsamp++;
			if ($nsamp > $plotnrows  or $first==1){
				$nsamp=1;
				$nout++;
				if ($first != 1){
					print RSCR "plotlog<-dev.off()\n";
				}else{
					$first=0;
				}
				while ($created == 1){
					$pngfile = "flowclust-$nout.png";
					$pngfiles.=" flowclust-$nout.png";
					if (-e $pngfile){
						$nout++;
					}else{
						$created=0;
					}
				}
				print RSCR "png(file='flowclust-$nout.png',pointsize=25,width=2480,height=3508)\n";
				print RSCR "par(mfrow = c($plotnrows,$ncol))\n";
			}
		}
		print RSCR "sel_$clg2_ch1<-exprs(flowdata\$${clg2_ch1}\[flowdata\$$clg2_ch1>$clg2_min & flowdata\$$clg2_ch1<$clg2_ch1_max\])\n";
		print RSCR "sel_$clg2_ch2<-exprs(flowdata\$${clg2_ch2}\[flowdata\$$clg2_ch2>$clg2_min & flowdata\$$clg2_ch2<$clg2_ch2_max\])\n";

		print RSCR "hist_$clg2_ch1<-hist(sel_$clg2_ch1,breaks=1000,plot = FALSE)\n";
		print RSCR "hist_$clg2_ch2<-hist(sel_$clg2_ch2,breaks=1000,plot = FALSE)\n";
		print RSCR "${clg2_ch1}.max<-min(head(hist_${clg2_ch1}\$breaks[cumsum(hist_${clg2_ch1}\$counts)/ nrow(sel_$clg2_ch1) > $clg2_th],1),$clg2_ch1_max)\n";
		print RSCR "${clg2_ch2}.max<-min(head(hist_${clg2_ch2}\$breaks[cumsum(hist_${clg2_ch2}\$counts)/ nrow(sel_$clg2_ch2) > $clg2_th],1),$clg2_ch2_max)\n";

		for $nc(1..$clg2_nc){
			print RSCR "clfilter$nc <- tmixFilter('clfilter$nc', c('$clg2_ch1', '$clg2_ch2'), K = $nc, randomStart=10, B = 100,level=$clg2_level,min=c($clg2_min,$clg2_min),max=c(${clg2_ch1}.max,${clg2_ch2}.max))\n";
			$resname="res_mult_${clg2_ch1}_${clg2_ch2}_$nc";
			print RSCR "$resname<-filter(flowdata,clfilter$nc)\n";
			$pl_title="pl_title<-paste('$sample BIC:',round(criterion($resname,'BIC')),'\\nProp:',toString(round(slot($resname,'w'),2))";
			if ($nc > 1){
				print RSCR "bicratio<-(bic1/criterion($resname,'BIC')-1)*100\n";
				$pl_title.=",'\\nBIC ratio:',round(bicratio,2), sep = ' ')\n";
			}else{
				$pl_title.=", sep = ' ')\n";
				print RSCR "bic1<-criterion($resname,'BIC')\n";
			}
			print RSCR $pl_title;
			print RSCR "plotlog<-plot($resname,level=$clg2_level,z.cutoff=0,data=flowdata,xlim=c(min(sel_$clg2_ch1),${clg2_ch1}.max*1.1),ylim=c(min(sel_$clg2_ch2),${clg2_ch2}.max*1.1),main=pl_title)\n";	
			#print RSCR "flowdata_split_${clg1_ch1}_${clg1_ch2}_$nc<-split(flowdata,res1_mult_${clg1_ch1}_${clg1_ch2}_$nc,population = list(sc1 = 1\n";
			#for $nc2(2..$nc){
			#	print RSCR ",sc$nc2=$nc2";
			#}
			#print RSCR "))\n";
			#print RSCR "ggplot(data=flowdata_split$sc4, aes(x=FSC.A,y=SSC.A))+geom_bin2d(bins = 500) +scale_fill_continuous(type = "viridis") +theme_bw()
		}
		$resname1="res_mult_${clg2_ch1}_${clg2_ch2}_1";
		if ($clg2_nc > 1){
			$resname2="res_mult_${clg2_ch1}_${clg2_ch2}_2";
			print RSCR "if (bicratio>$clg2_bic_th){\n";
			print RSCR "\tcl<-which.max(slot($resname2,'w'))\n";
			print RSCR "\tflowdata_comp_log<-split(flowdata,$resname2,population = list(sc1 = cl))[[1]]\n";
			print RSCR "\}else{\n";
			print RSCR "\tflowdata_comp_log<-split(flowdata,$resname1)[[1]]\n";
			print RSCR "}\n";

		}else{
			print RSCR "flowdata_comp_log<-split(flowdata,$resname1)[[1]]\n";
		}
	}
	if ($filter2 == 1){
		print RSCR "flowdata_comp_log_f2<-Subset(flowdata_comp_log,filter(flowdata_comp_log,n2f2))\n";
		print RSCR "flowdata_comp_log<-exprs(flowdata_comp_log_f2[,c('$ch1','$ch2','${ch1}_log','${ch2}_log')])\n";
	}else{
		##print RSCR "flowdata_sc_comp<-compensate(flowdata_sc,spillover(flowdata_sc)[[1]])\n";
		#print RSCR "flowdata_comp_log<-exprs(transform(compensate(flowdata,spillover(flowdata)[[1]]),`${ch1}_log`=log(`$ch1`),`${ch2}_log`=log(`$ch2`))[,c('$ch1','$ch2','${ch1}_log','${ch2}_log')])\n";
		print RSCR "flowdata_comp_log<-exprs(flowdata_comp_log[,c('$ch1','$ch2','${ch1}_log','${ch2}_log')])\n"
		##print RSCR "flowdata_sc_comp_log<-transform(flowdata_sc_comp,`FL2.A_log`=log(`FL2.A`),`FL7.A_log`=log(`FL7.A`),`SSC.A_log`=log(`SSC.A`),`FSC.A_log`=log(`FSC.A`))\n"
	}
	print RSCR "colnames(flowdata_comp_log)<-c('$ch1','$ch2','log$ch1','log$ch2')\n";
	print RSCR "datlog<-as.data.frame(flowdata_comp_log[is.finite(rowSums(flowdata_comp_log)),])\n";
	print RSCR "write.csv(datlog,file='$csv',row.names=FALSE,quote=FALSE)\n";

	if ($lowfilter != 0){
		print RSCR "datlog<-datlog[datlog\$log$ch1>$lowfilter,]\n";
	}
	print RSCR "fit.datlog.em.$type<-EmSkew(datlog[,c(3,4)],$nmix,distr='$type',debug=FALSE)\n";
	#if ($firstsample==1){
	#	$firstsample=0;
	#	print RSCR "S2<-data.frame('Sample'='$rname-lab.png','S2'=fit.datlog.em.mvn\$sigma[2],stringsAsFactors = FALSE)\n";
	#}else{
	#	print RSCR "S2<-rbind(S2,list('$rname-lab.png',fit.datlog.em.mvn\$sigma[2]))\n";
	#}
	print RSCR "plotlog<-EmSkew.flow(datlog[,c(3,4)],fit.datlog.em.$type,lower.panel='type4',title='$sample.$rname',path=getwd(),plot=FALSE)\n";
	print RSCR "empng<-load.image('$sample.$rname.png')\n";
	print RSCR "plot(empng,axes=FALSE)\n";
	if ($type eq 'mvn'){
		print RSCR "cat ('$sample.$rname',fit.datlog.em.$type\$mu,fit.datlog.em.$type\$sigma[$sigmas]";
	}else{
		print RSCR "cat ('$sample.$rname',fit.datlog.em.$type\$mu,fit.datlog.em.$type\$sigma[$sigmas],fit.datlog.em.$type\$delta";
	}
	if($clusterg1==1 and $clg1_nc==2){
		$resname="res_mult_${clg1_ch1}_${clg1_ch2}_2";
		print RSCR ",criterion($resname,'BIC'),criterion($resname,'ICL'),criterion($resname,'logLike')";
	}
	if($clusterg2==1){
		$resname1="res_mult_${clg1_ch1}_${clg1_ch2}_1";
		print RSCR ",criterion($resname1,'BIC'),criterion($resname1,'ICL'),criterion($resname1,'logLike')";
	}
	print RSCR ",sep='\\t')\n";
	print RSCR "cat ('\\n')\n";

}
if ($clusterg1 or $clusterg2){
	print RSCR "plotlog<-dev.off()\n";
	print RSCR "cmd<-'/bin/pdfjam --outfile sample_gating.pdf $pngfiles'\n";
	print RSCR "system(cmd,ignore.stdout = TRUE, ignore.stderr = TRUE)\n";
}
print RSCR "sink()\n";
#exit;
print STDERR "Loading R libraries\n";
`Rscript extract_and_class_FCDat.R`;
`grep -v ^Rule clg1-clg2-log-noneg.csv > tmp.csv; \\mv tmp.csv clg1-clg2-log-noneg.csv`;
