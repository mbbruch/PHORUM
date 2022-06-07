appendEAFs <- function(phorum_new,phorum_old){
     
     phorum.old[,':='(size.bucket=fifelse(pmax(gen.capacity.winter,gen.capacity.summer)>=500,
                                          "500+",fifelse(pmax(gen.capacity.winter,gen.capacity.summer)>=250,"250-499","0-250")
     ))]
     eafs.jan <- phorum.old[,.(eaf=mean(`EAF Jan-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.feb <- phorum.old[,.(eaf=mean(`EAF Feb-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.mar <- phorum.old[,.(eaf=mean(`EAF Mar-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.apr <- phorum.old[,.(eaf=mean(`EAF Apr-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.may <- phorum.old[,.(eaf=mean(`EAF May-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.jun <- phorum.old[,.(eaf=mean(`EAF Jun-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.jul <- phorum.old[,.(eaf=mean(`EAF Jul-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.aug <- phorum.old[,.(eaf=mean(`EAF Aug-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.sep <- phorum.old[,.(eaf=mean(`EAF Sept-10`),na.rm=TRUE),by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.oct <- phorum.old[,.(eaf=mean(`EAF Oct-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.nov <- phorum.old[,.(eaf=mean(`EAF Nov-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     eafs.dec <- phorum.old[,.(eaf=mean(`EAF Dec-10`),na.rm=TRUE), by=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     
     phorum_new[,':='(
          EAF_01=100,
          EAF_02=100,
          EAF_03=100,
          EAF_04=100,
          EAF_05=100,
          EAF_06=100,
          EAF_07=100,
          EAF_08=100,
          EAF_09=100,
          EAF_10=100,
          EAF_11=100,
          EAF_12=100,
          size.bucket=fifelse(gen.capacity.needs>=500,
                              "500+",fifelse(gen.capacity.needs>=250,"250-499","0-250")
          ))]
     phorum_new[eafs.jan,EAF_01:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.feb,EAF_02:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.mar,EAF_03:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.apr,EAF_04:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.may,EAF_05:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.jun,EAF_06:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.jul,EAF_07:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.aug,EAF_08:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.sep,EAF_09:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.oct,EAF_10:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.nov,EAF_11:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     phorum_new[eafs.dec,EAF_12:=eaf,on=.(size.bucket,detailed.type, unit.type, gen.type.needs, fuel)]
     
     return(phorum_new);
}