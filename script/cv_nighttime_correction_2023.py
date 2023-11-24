#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep  9 10:28:56 2021

@author: katie
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec 15 11:42:52 2020

@author: katie
"""

# -*- coding: utf-8 -*-
"""
Created on Wed Dec  2 11:24:56 2020

@author: chmlkw
"""

#programme to convert spec rad signal to actinic flux (with methyl iodide and other halocarbon rates)

import numpy as np
import os
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from datetime import datetime, timedelta
import math

radian=180/math.pi

outfile=open("CV_2023-08_39s_R_CH3I_HALOGENS.txt", "wt")
outfile.write("Time and Date"+","+"j(o1d)"+","+"j(o3p)"+","+"j(h2o2)"+","+"j(no2)"+","+"j(no3tono)"+","+"j(no3tono2)"+","+"j(hono)"+","+"j(hno3)"+","+"j(hchor)"+","+"j(hchonr)"+","+"j(ch3cho)"+","+"j(c2h5cho)"+","+"j15(c3h7cho)"+","+"j16(c2h5cho)"+","+"j(iprcho)"+","+"j18(macr)"+","+"j19(macr)"+","+"j(hpald)"+","+"j(ch3coch3)"+","+"j(mek)"+","+"j23(mvk)"+","+"j24(mvk)"+","+"j31(glyox)"+","+"j32(glyox)"+","+"j33(glyox)"+","+"j(mglyox)"+","+"j(biacet)"+","+"j(ch3ooh)"+","+"j(ch3no3)"+","+"j(c2h5no3)"+","+"j(nc3h7no3)"+","+"j(ic3h7no3)"+","+"j(tc4h9no3)"+","+"j(noa)"+","+"j(clno2)"+","+"j(clono2a)"+","+"j(clono2b)"+","+"j(hocl)"+","+"j(ch3i)"+","+"j(bro)"+","+"j(hobr)"+","+"j(brno3_no2)"+","+"j(brno3_no3)"+","+"j(brno2)"+","+"j(hoi)"+","+"j(io)"+","+"j(oio)"+","+"m"+","+"m_err"+","+"c0"+","+"c0_err"+","+"sza\n") 

longit=float(input("Enter Longitude: "))
lat=float(input("Enter Latitude: "))
diff=float(input("Enter time difference from UTC: "))
direct=input("Enter directory: ")
txtfiles=os.listdir(direct)
for root, subFolders, files in os.walk(direct):
    for file in files:

        
#for item in txtfiles:
    #file=direct+item
    
            I=[]
            w=[]
            wvl =[]
            D=[]
            wvlabs=[]
            xsn=[] 
            qy=[]
            wvlabsjno2=[]
            xsnjno2=[] 
            qyjno2=[]
            wvlabsjhono=[]
            xsnjhono=[] 
            qyjhono=[]
            wvlabsjhchor=[]
            xsnjhchor=[] 
            qyjhchor=[]
            wvlabsjhchonr=[]
            xsnjhchonr=[] 
            qyjhchonr=[]
            wvlabsjno3tono=[]
            xsnjno3tono=[] 
            qyjno3tono=[]
            wvlabsjno3tono2=[]
            xsnjno3tono2=[] 
            qyjno3tono2=[]
            wvlabsjch3cho=[]
            xsnjch3cho=[] 
            qyjch3cho=[]
            wvlabsjch3coch3=[]
            xsnjch3coch3=[] 
            qyjch3coch3=[]
            wvlabsjclno2=[]
            xsnjclno2=[] 
            qyjclno2=[]
            wvlabsjclono2a=[]
            xsnjclono2a=[] 
            qyjclono2a=[]
            wvlabsjclono2b=[]
            xsnjclono2b=[] 
            qyjclono2b=[]
            wvlabsjhocl=[]
            xsnjhocl=[] 
            qyjhocl=[]
            wvltrue=[]
            zero=[]
            Id=[]
            Wd=[]
            timeh=[]
            timem=[]
            times=[]
            date=[]
            sza=[]
            wvlabsjh2o2=[]
            xsnjh2o2=[] 
            qyjh2o2=[]
            wvlabsjhno3=[]
            xsnjhno3=[] 
            qyjhno3=[]
            wvlabsjc2h5cho=[]
            xsnjc2h5cho=[] 
            qyjc2h5cho=[]
            wvlabsj15c3h7cho=[]
            xsnj15c3h7cho=[] 
            qyj15c3h7cho=[]
            wvlabsj16c3h7cho=[]
            xsnj16c3h7cho=[] 
            qyj16c3h7cho=[]
            wvlabsjiprcho=[]
            xsnjiprcho=[] 
            qyjiprcho=[]
            wvlabsj18macr=[]
            xsnj18macr=[] 
            qyj18macr=[]
            wvlabsj19macr=[]
            xsnj19macr=[] 
            qyj19macr=[]
            wvlabsjhpald=[]
            xsnjhpald=[] 
            qyjhpald=[]
            wvlabsjmek=[]
            xsnjmek=[] 
            qyjmek=[]
            wvlabsj23mvk=[]
            xsnj23mvk=[] 
            qyj23mvk=[]
            wvlabsj24mvk=[]
            xsnj24mvk=[] 
            qyj24mvk=[]
            wvlabsjmglyox=[]
            xsnjmglyox=[] 
            qyjmglyox=[]
            wvlabsjbiacet=[]
            xsnjbiacet=[] 
            qyjbiacet=[]
            wvlabsjch3ooh=[]
            xsnjch3ooh=[] 
            qyjch3ooh=[]
            wvlabsjch3no3=[]
            xsnjch3no3=[] 
            qyjch3no3=[]
            wvlabsjc2h5no3=[]
            xsnjc2h5no3=[] 
            qyjc2h5no3=[]
            wvlabsjnc3h7no3=[]
            xsnjnc3h7no3=[] 
            qyjnc3h7no3=[]
            wvlabsjic3h7no3=[]
            xsnjic3h7no3=[] 
            qyjic3h7no3=[]
            wvlabsjtc4h9no3=[]
            xsnjtc4h9no3=[] 
            qyjtc4h9no3=[]
            wvlabsjnoa=[]
            xsnjnoa=[] 
            qyjnoa=[]
            wvlabsj31glyox=[]
            xsnj31glyox=[] 
            qyj31glyox=[]
            wvlabsj32glyox=[]
            xsnj32glyox=[] 
            qyj32glyox=[]
            wvlabsj33glyox=[]
            xsnj33glyox=[] 
            qyj33glyox=[]
            wvlabsj2o3=[]
            xsnj2o3=[] 
            qyj2o3=[]
            wvlabsjch3i=[]
            xsnjch3i=[] 
            qyjch3i=[]
            wvlabsjbro=[]
            xsnjbro=[]
            qyjbro=[]
            wvlabsjhobr=[]
            xsnjhobr=[]
            qyjhobr=[]
            wvlabsjbrno3_no2=[]
            xsnjbrno3_no2=[]
            qyjbrno3_no2=[]
            wvlabsjbrno3_no3=[]
            xsnjbrno3_no3=[]
            qyjbrno3_no3=[]
            wvlabsjbrno2=[]
            xsnjbrno2=[]
            qyjbrno2=[]
            wvlabsjhoi=[]
            xsnjhoi=[]
            qyjhoi=[]
            wvlabsjio=[]
            xsnjio=[]
            qyjio=[]
            wvlabsjoio=[]
            xsnjoio=[]
            qyjoio=[]
            
            with open(os.path.join(root, file), 'r') as infile:
     
    
        
    
        #with open(file , "rt") as infile:
    
                for n in range(2):        
                    infile.readline()
    #            for line in range(1):
    #                time=infile.readline()
    #                t=time.split(" ")
    
                    
                for line in range(1):
                    line=infile.readline()
                    t=line.split(" ")
                    #time=t[4]
                    time = datetime.strptime(t[4], '%H:%M:%S')
                    utc= time+timedelta(hours=diff)                 
                    format(utc, '%H:%M:%S')
                    h=utc.strftime('%H')
                    hs=3600*float(h)
                    m=utc.strftime('%M')
                    ms=60*float(m)
                    s=utc.strftime('%S')
                    ss=float(s)
                    #print(utc)
    
                    time_s=hs+ms+ss
                    fd=(hs+ms+ss)/(24*3600)
                    #print(fd)
                    date=t[2]+" "+t[3]+" 2018"
                    
                    #print(date)
                    dateyear = datetime.strptime(date, "%b %d %Y")
                    dateyear.strftime("%m/%d/%Y")
                    tt=dateyear.timetuple()
                    tt.tm_yday
                    latrad=math.pi/180*lat
                    longrad=math.pi/180*longit
                    fyear=(tt.tm_yday/365)
                    xyear=fyear+(time_s/(3.6525E+02*2.40E+01*3.60E+03))
                    fsun=1+(3.4E-02*math.cos(2*math.pi*fyear))  
                    dec=-0.4*math.cos(2*math.pi*(tt.tm_yday+10)/365)
                    #dec=math.pi/180*decdeg
                    #((-23.45*math.pi)/180)*math.cos(2*math.pi*(tt.tm_yday+10)/365)
                    #dec=-4.1420E-01*math.cos(2.00E+00*math.pi*xyear)
                    #lha=(1.00E+00+time_s/4.32E+04)*math.pi+longrad
                    
                    #cosld=math.cos(latrad)*math.cos(dec)
                    #sinld=math.sin(latrad)*math.sin(dec)
                    #ct=(math.cos(lha)*cosld)+sinld
                    #zennow=radian*math.atan((math.sqrt(1-ct*ct))/ct)
    
                    #print(fyear)
                    f=(math.pi/180)*(279.5+0.985*fyear)
                    et=(-104.7*math.sin(f)+596.2*math.sin(2*f)+4.3*math.sin(3*f)-12.7*math.sin(4*f)-429.3*math.cos(f)-2*math.cos(2*f)+19.3*math.cos(3*f))/3600
                    t0=(12/24)-((4/1440)*longit)-et
                    lha=math.pi*(fd-t0)/0.5
    #      #              xyear=((float(record[0]))-41638)/365
    #
    #
    #                latrad=math.pi/180*lat
                    ct=math.sin(latrad)*math.sin(dec)+math.cos(latrad)*math.cos(dec)*math.cos(lha)
                    zennow=radian*math.atan((math.sqrt(1-ct*ct))/ct)
    ##
                    if ct<0:
                            zennow=180+zennow
    #                        sza.append(zennow)
    #
                    if zennow>86:
                           zennow=86
                    zennowrad=zennow*(math.pi/180)
                    amf=(float(1/math.cos(zennowrad)))
    #                
                    #print(zennow)
                for n in range(3):
                    infile.readline()
                    
                for line in range(1):
                    integration=infile.readline()
                    s=integration.split(" ")
    #                filenamea.append(t[2])
    #                filenameb.append(t[3])
    #                filenamec=t[4].split(":")
    #                outfile=open(filenamea[0]+filenameb[0]+filenamec[0]+filenamec[1]+filenamec[2]+".txt", "wt")
    
                for n in range(100):
                    infile.readline()
                
                for n in range(1):
                    Ix=infile.readline()
                    I0=Ix.split()
                    zero.append(I0[1])
    
                for line in infile:
                    record=line.split()
                    I.append(record[1])
                    w.append(record[0])
    
       
            with open ("cv_cal_2019.txt", "rt") as infile:
            #with open ("cal_27072016.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
            
    
                for line in infile:
                    record=line.split()
                    wvl.append(record[0])
                    D.append(float(record[1]))
                    
                    
            with open ("3.txt", "rt") as infile:
                for n in range(108):
                    infile.readline()
                for line in infile:
                    record=line.split()
                    Id.append(record[1])
                    Wd.append(record[0])
                    Idmod=Id[:-331]
                    Wdmod=Wd[:-331]
    
    
    
            with open ("absqyo3_julich.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabs.append(float(record[0]))
                    xsn.append(float(record[1]))
                    qy.append(float(record[2]))
    
                    darray=np.array(D, float)
                    wvlarray=np.array(wvl, float)
                    wvltrue=wvlarray+0                
                    wvldoub=wvlarray*2
    
    
                    
    
                    
                    
                   # wvlarray=np.array(wvl, float)
                    xsnintp=np.interp(wvltrue, wvlabs, xsn)
                    qyintp=np.interp(wvltrue, wvlabs, qy)
                    Imod=I[:-331]
                    Wmod=w[:-331]
                    warray=np.array(Wmod, float)
                    iarray=np.array(Imod, float)
                    idarray=np.array(Idmod, float)
                    isarray=iarray-idarray
                    #Idoub=np.interp(wvldoub, wvl, Imod)
                    #Iddoub=np.interp(wvldoub, wvl, Idmod)
                    #Itrue=np.interp(wvltrue, wvl, Imod)
                    #Idtrue=np.interp(wvl, wvl, Idmod)
                    
                    
    #                wvltrue.tolist()
    
    #                Idoubmod=Idoub[:-338]
    #                extra=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    #               for n in extra:
    #                   Idoubmod.append(n)
    
           
            with open ("absqyNO2_julich.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjno2.append(float(record[0]))
                    xsnjno2.append(float(record[1]))
                    qyjno2.append(float(record[2]))
    #                darray=np.array(D, float)
    #                wvlarray=np.array(wvl, float)
    #                wvldoub=wvlarray*2
    #                wvltrue=wvlarray+7.0
                   # wvlarray=np.array(wvl, float)
                    xsnintpjno2=np.interp(wvltrue, wvlabsjno2, xsnjno2)
                    qyintpjno2=np.interp(wvltrue, wvlabsjno2, qyjno2)
    #                wvltrue.tolist(
    #                Imod=I[:-338]
    #                Wmod=w[:-338]
    #                Idoub=np.interp(wvldoub, wvl, Imod)
    #                Idoubmod=Idoub[:-338]
    #                extra=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    #               for n in extra:
    #                   Idoubmod.append(n)
    #            iarray=np.array(Imod, float)
    #            idoubarray=np.array(Idoub, float)
    #            darray=np.array(D, float)
    #            warray=np.array(Wmod, float)
    #            flux = ((((iarray-(float(I0[1])))-(idoubarray*0.00045))/darray))*1.53
            with open ("absqyHONO_jpl11.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhono.append(float(record[0]))
                    xsnjhono.append(float(record[1]))
                    qyjhono.append(float(record[2]))
                    xsnintpjhono=np.interp(wvltrue, wvlabsjhono, xsnjhono)
                    qyintpjhono=np.interp(wvltrue, wvlabsjhono, qyjhono)
            with open ("absqyHCHOr_IUPACb.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhchor.append(float(record[0]))
                    xsnjhchor.append(float(record[1]))
                    qyjhchor.append(float(record[2]))
                    xsnintpjhchor=np.interp(wvltrue, wvlabsjhchor, xsnjhchor)
                    qyintpjhchor=np.interp(wvltrue, wvlabsjhchor, qyjhchor)
            with open ("absqyHCHOnr_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhchonr.append(float(record[0]))
                    xsnjhchonr.append(float(record[1]))
                    qyjhchonr.append(float(record[2]))
                    xsnintpjhchonr=np.interp(wvltrue, wvlabsjhchonr, xsnjhchonr)
                    qyintpjhchonr=np.interp(wvltrue, wvlabsjhchonr, qyjhchonr)
            with open ("absqyNO3_NO_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjno3tono.append(float(record[0]))
                    xsnjno3tono.append(float(record[1]))
                    qyjno3tono.append(float(record[2]))
                    xsnintpjno3tono=np.interp(wvltrue, wvlabsjno3tono, xsnjno3tono)
                    qyintpjno3tono=np.interp(wvltrue, wvlabsjno3tono, qyjno3tono)
            with open ("absqyNO3_NO2_IUPAC_revised.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjno3tono2.append(float(record[0]))
                    xsnjno3tono2.append(float(record[1]))
                    qyjno3tono2.append(float(record[2]))
                    xsnintpjno3tono2=np.interp(wvltrue, wvlabsjno3tono2, xsnjno3tono2)
                    qyintpjno3tono2=np.interp(wvltrue, wvlabsjno3tono2, qyjno3tono2)
            with open ("absqyCH3CHO_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjch3cho.append(float(record[0]))
                    xsnjch3cho.append(float(record[1]))
                    qyjch3cho.append(float(record[2]))
                    xsnintpjch3cho=np.interp(wvltrue, wvlabsjch3cho, xsnjch3cho)
                    qyintpjch3cho=np.interp(wvltrue, wvlabsjch3cho, qyjch3cho)
            with open ("absqyCH3COCH3_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjch3coch3.append(float(record[0]))
                    xsnjch3coch3.append(float(record[1]))
                    qyjch3coch3.append(float(record[2]))
                    xsnintpjch3coch3=np.interp(wvltrue, wvlabsjch3coch3, xsnjch3coch3)
                    qyintpjch3coch3=np.interp(wvltrue, wvlabsjch3coch3, qyjch3coch3)
                    
            with open ("absclno2.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjclno2.append(float(record[0]))
                    xsnjclno2.append(float(record[1]))
                    qyjclno2.append(float(record[2]))
                    xsnintpjclno2=np.interp(wvltrue, wvlabsjclno2, xsnjclno2)
                    qyintpjclno2=np.interp(wvltrue, wvlabsjclno2, qyjclno2)
                    
            with open ("absqyCLONO2a_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjclono2a.append(float(record[0]))
                    xsnjclono2a.append(float(record[1]))
                    qyjclono2a.append(float(record[2]))
                    xsnintpjclono2a=np.interp(wvltrue, wvlabsjclono2a, xsnjclono2a)
                    qyintpjclono2a=np.interp(wvltrue, wvlabsjclono2a, qyjclono2a)
                    
            with open ("absqyCLONO2b_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjclono2b.append(float(record[0]))
                    xsnjclono2b.append(float(record[1]))
                    qyjclono2b.append(float(record[2]))
                    xsnintpjclono2b=np.interp(wvltrue, wvlabsjclono2b, xsnjclono2b)
                    qyintpjclono2b=np.interp(wvltrue, wvlabsjclono2b, qyjclono2b)
                    
            with open ("absqyHOCL_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhocl.append(float(record[0]))
                    xsnjhocl.append(float(record[1]))
                    qyjhocl.append(float(record[2]))
                    xsnintpjhocl=np.interp(wvltrue, wvlabsjhocl, xsnjhocl)
                    qyintpjhocl=np.interp(wvltrue, wvlabsjhocl, qyjhocl)
            
            with open ("absqyH2O2_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjh2o2.append(float(record[0]))
                    xsnjh2o2.append(float(record[1]))
                    qyjh2o2.append(float(record[2]))
                    xsnintpjh2o2=np.interp(wvltrue, wvlabsjh2o2, xsnjh2o2)
                    qyintpjh2o2=np.interp(wvltrue, wvlabsjh2o2, qyjh2o2)
                    
            with open ("absqyHNO3_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhno3.append(float(record[0]))
                    xsnjhno3.append(float(record[1]))
                    qyjhno3.append(float(record[2]))
                    xsnintpjhno3=np.interp(wvltrue, wvlabsjhno3, xsnjhno3)
                    qyintpjhno3=np.interp(wvltrue, wvlabsjhno3, qyjhno3)
                    
            with open ("absqyC2H5CHO_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjc2h5cho.append(float(record[0]))
                    xsnjc2h5cho.append(float(record[1]))
                    qyjc2h5cho.append(float(record[2]))
                    xsnintpjc2h5cho=np.interp(wvltrue, wvlabsjc2h5cho, xsnjc2h5cho)
                    qyintpjc2h5cho=np.interp(wvltrue, wvlabsjc2h5cho, qyjc2h5cho)
                    
            with open ("absqyC3H7CHO_j15_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj15c3h7cho.append(float(record[0]))
                    xsnj15c3h7cho.append(float(record[1]))
                    qyj15c3h7cho.append(float(record[2]))
                    xsnintpj15c3h7cho=np.interp(wvltrue, wvlabsj15c3h7cho, xsnj15c3h7cho)
                    qyintpj15c3h7cho=np.interp(wvltrue, wvlabsj15c3h7cho, qyj15c3h7cho)
                    
            with open ("absqyC3H7CHO_j16_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj16c3h7cho.append(float(record[0]))
                    xsnj16c3h7cho.append(float(record[1]))
                    qyj16c3h7cho.append(float(record[2]))
                    xsnintpj16c3h7cho=np.interp(wvltrue, wvlabsj16c3h7cho, xsnj16c3h7cho)
                    qyintpj16c3h7cho=np.interp(wvltrue, wvlabsj16c3h7cho, qyj16c3h7cho)
                    
            with open ("absqyIPRCHO_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjiprcho.append(float(record[0]))
                    xsnjiprcho.append(float(record[1]))
                    qyjiprcho.append(float(record[2]))
                    xsnintpjiprcho=np.interp(wvltrue, wvlabsjiprcho, xsnjiprcho)
                    qyintpjiprcho=np.interp(wvltrue, wvlabsjiprcho, qyjiprcho)
                    
            with open ("absqyMACR_j18_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj18macr.append(float(record[0]))
                    xsnj18macr.append(float(record[1]))
                    qyj18macr.append(float(record[2]))
                    xsnintpj18macr=np.interp(wvltrue, wvlabsj18macr, xsnj18macr)
                    qyintpj18macr=np.interp(wvltrue, wvlabsj18macr, qyj18macr)
                    
            with open ("absqyMACR_j19_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj19macr.append(float(record[0]))
                    xsnj19macr.append(float(record[1]))
                    qyj19macr.append(float(record[2]))
                    xsnintpj19macr=np.interp(wvltrue, wvlabsj19macr, xsnj19macr)
                    qyintpj19macr=np.interp(wvltrue, wvlabsj19macr, qyj19macr)
                    
            with open ("absqyHPALD_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhpald.append(float(record[0]))
                    xsnjhpald.append(float(record[1]))
                    qyjhpald.append(float(record[2]))
                    xsnintpjhpald=np.interp(wvltrue, wvlabsjhpald, xsnjhpald)
                    qyintpjhpald=np.interp(wvltrue, wvlabsjhpald, qyjhpald)
                    
            with open ("absqyMEK_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjmek.append(float(record[0]))
                    xsnjmek.append(float(record[1]))
                    qyjmek.append(float(record[2]))
                    xsnintpjmek=np.interp(wvltrue, wvlabsjmek, xsnjmek)
                    qyintpjmek=np.interp(wvltrue, wvlabsjmek, qyjmek)
                    
            with open ("absqyMVK_j23_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj23mvk.append(float(record[0]))
                    xsnj23mvk.append(float(record[1]))
                    qyj23mvk.append(float(record[2]))
                    xsnintpj23mvk=np.interp(wvltrue, wvlabsj23mvk, xsnj23mvk)
                    qyintpj23mvk=np.interp(wvltrue, wvlabsj23mvk, qyj23mvk)
                    
            with open ("absqyMVK_j24_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj24mvk.append(float(record[0]))
                    xsnj24mvk.append(float(record[1]))
                    qyj24mvk.append(float(record[2]))
                    xsnintpj24mvk=np.interp(wvltrue, wvlabsj24mvk, xsnj24mvk)
                    qyintpj24mvk=np.interp(wvltrue, wvlabsj24mvk, qyj24mvk)
                    
            with open ("absqyMGLYOX_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjmglyox.append(float(record[0]))
                    xsnjmglyox.append(float(record[1]))
                    qyjmglyox.append(float(record[2]))
                    xsnintpjmglyox=np.interp(wvltrue, wvlabsjmglyox, xsnjmglyox)
                    qyintpjmglyox=np.interp(wvltrue, wvlabsjmglyox, qyjmglyox)
                    
            with open ("absqyBIACET_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjbiacet.append(float(record[0]))
                    xsnjbiacet.append(float(record[1]))
                    qyjbiacet.append(float(record[2]))
                    xsnintpjbiacet=np.interp(wvltrue, wvlabsjbiacet, xsnjbiacet)
                    qyintpjbiacet=np.interp(wvltrue, wvlabsjbiacet, qyjbiacet)
                    
            with open ("absqyCH3OOH_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjch3ooh.append(float(record[0]))
                    xsnjch3ooh.append(float(record[1]))
                    qyjch3ooh.append(float(record[2]))
                    xsnintpjch3ooh=np.interp(wvltrue, wvlabsjch3ooh, xsnjch3ooh)
                    qyintpjch3ooh=np.interp(wvltrue, wvlabsjch3ooh, qyjch3ooh)
                    
            with open ("absqyCH3NO3_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjch3no3.append(float(record[0]))
                    xsnjch3no3.append(float(record[1]))
                    qyjch3no3.append(float(record[2]))
                    xsnintpjch3no3=np.interp(wvltrue, wvlabsjch3no3, xsnjch3no3)
                    qyintpjch3no3=np.interp(wvltrue, wvlabsjch3no3, qyjch3no3)
                    
            with open ("absqyC2H5NO3_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjc2h5no3.append(float(record[0]))
                    xsnjc2h5no3.append(float(record[1]))
                    qyjc2h5no3.append(float(record[2]))
                    xsnintpjc2h5no3=np.interp(wvltrue, wvlabsjc2h5no3, xsnjc2h5no3)
                    qyintpjc2h5no3=np.interp(wvltrue, wvlabsjc2h5no3, qyjc2h5no3)
                    
            with open ("absqyNC3H7NO3_mcm.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjnc3h7no3.append(float(record[0]))
                    xsnjnc3h7no3.append(float(record[1]))
                    qyjnc3h7no3.append(float(record[2]))
                    xsnintpjnc3h7no3=np.interp(wvltrue, wvlabsjnc3h7no3, xsnjnc3h7no3)
                    qyintpjnc3h7no3=np.interp(wvltrue, wvlabsjnc3h7no3, qyjnc3h7no3)
                    
                    
            with open ("absqyIC3H7NO3_mcm.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjic3h7no3.append(float(record[0]))
                    xsnjic3h7no3.append(float(record[1]))
                    qyjic3h7no3.append(float(record[2]))
                    xsnintpjic3h7no3=np.interp(wvltrue, wvlabsjic3h7no3, xsnjic3h7no3)
                    qyintpjic3h7no3=np.interp(wvltrue, wvlabsjic3h7no3, qyjic3h7no3)
             
            with open ("absqyTC4H9NO3_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjtc4h9no3.append(float(record[0]))
                    xsnjtc4h9no3.append(float(record[1]))
                    qyjtc4h9no3.append(float(record[2]))
                    xsnintpjtc4h9no3=np.interp(wvltrue, wvlabsjtc4h9no3, xsnjtc4h9no3)
                    qyintpjtc4h9no3=np.interp(wvltrue, wvlabsjtc4h9no3, qyjtc4h9no3)
                    
            with open ("absqyNOA_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjnoa.append(float(record[0]))
                    xsnjnoa.append(float(record[1]))
                    qyjnoa.append(float(record[2]))
                    xsnintpjnoa=np.interp(wvltrue, wvlabsjnoa, xsnjnoa)
                    qyintpjnoa=np.interp(wvltrue, wvlabsjnoa, qyjnoa)
                    
            
                wvlabsjnoaarray=np.array(wvlabsjnoa, float)
                xsnjnoaarray=np.array(xsnjnoa, float)
                
            with open ("absqyGLYOX_H2_2CO.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj31glyox.append(float(record[0]))
                    xsnj31glyox.append(float(record[1]))
                    qyj31glyox.append(float(record[2]))
                    xsnintpj31glyox=np.interp(wvltrue, wvlabsj31glyox, xsnj31glyox)
                    qyintpj31glyox=np.interp(wvltrue, wvlabsj31glyox, qyj31glyox)
                    
            
                wvlabsj31glyoxarray=np.array(wvlabsj31glyox, float)
                xsnj31glyoxarray=np.array(xsnj31glyox, float)
                
            with open ("absqyGLYOX_HCHO_CO.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj32glyox.append(float(record[0]))
                    xsnj32glyox.append(float(record[1]))
                    qyj32glyox.append(float(record[2]))
                    xsnintpj32glyox=np.interp(wvltrue, wvlabsj32glyox, xsnj32glyox)
                    qyintpj32glyox=np.interp(wvltrue, wvlabsj32glyox, qyj32glyox)
                    
            
                wvlabsj32glyoxarray=np.array(wvlabsj32glyox, float)
                xsnj32glyoxarray=np.array(xsnj32glyox, float)
                
            with open ("absqyGLYOX_HO2_2CO.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj33glyox.append(float(record[0]))
                    xsnj33glyox.append(float(record[1]))
                    qyj33glyox.append(float(record[2]))
                    xsnintpj33glyox=np.interp(wvltrue, wvlabsj33glyox, xsnj33glyox)
                    qyintpj33glyox=np.interp(wvltrue, wvlabsj33glyox, qyj33glyox)
                    
            
                wvlabsj33glyoxarray=np.array(wvlabsj33glyox, float)
                xsnj33glyoxarray=np.array(xsnj33glyox, float)
                
            with open ("absqyO3_j2_MCM.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsj2o3.append(float(record[0]))
                    xsnj2o3.append(float(record[1]))
                    qyj2o3.append(float(record[2]))
                    xsnintpj2o3=np.interp(wvltrue, wvlabsj2o3, xsnj2o3)
                    qyintpj2o3=np.interp(wvltrue, wvlabsj2o3, qyj2o3)
                    
            
                wvlabsj2o3array=np.array(wvlabsj2o3, float)
                xsnj2o3array=np.array(xsnj2o3, float)



            with open ("absqyCH3I_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjch3i.append(float(record[0]))
                    xsnjch3i.append(float(record[1]))
                    qyjch3i.append(float(record[2]))
                    xsnintpjch3i=np.interp(wvltrue, wvlabsjch3i, xsnjch3i)
                    qyintpjch3i=np.interp(wvltrue, wvlabsjch3i, qyjch3i)
                    
    
                wvlabsjch3iarray=np.array(wvlabsjch3i, float)
                xsnjch3iarray=np.array(xsnjch3i, float)
                    
    
            with open ("absqyBrO_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjbro.append(float(record[0]))
                    xsnjbro.append(float(record[1]))
                    qyjbro.append(float(record[2]))
                    xsnintpjbro=np.interp(wvltrue, wvlabsjbro, xsnjbro)
                    qyintpjbro=np.interp(wvltrue, wvlabsjbro, qyjbro)
                    
    
                wvlabsjbroarray=np.array(wvlabsjbro, float)
                xsnjbroarray=np.array(xsnjbro, float)                
                    
                   
            with open ("absqyHOBr_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhobr.append(float(record[0]))
                    xsnjhobr.append(float(record[1]))
                    qyjhobr.append(float(record[2]))
                    xsnintpjhobr=np.interp(wvltrue, wvlabsjhobr, xsnjhobr)
                    qyintpjhobr=np.interp(wvltrue, wvlabsjhobr, qyjhobr)
                    
    
                wvlabsjhobrarray=np.array(wvlabsjhobr, float)
                xsnjhobrarray=np.array(xsnjhobr, float)        


            with open ("absqyBrNO3_NO2_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjbrno3_no2.append(float(record[0]))
                    xsnjbrno3_no2.append(float(record[1]))
                    qyjbrno3_no2.append(float(record[2]))
                    xsnintpjbrno3_no2=np.interp(wvltrue, wvlabsjbrno3_no2, xsnjbrno3_no2)
                    qyintpjbrno3_no2=np.interp(wvltrue, wvlabsjbrno3_no2, qyjbrno3_no2)
                    
    
                wvlabsjbrno3_no2array=np.array(wvlabsjbrno3_no2, float)
                xsnjbrno3_no2array=np.array(xsnjbrno3_no2, float)       

            with open ("absqyBrNO3_NO3_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjbrno3_no3.append(float(record[0]))
                    xsnjbrno3_no3.append(float(record[1]))
                    qyjbrno3_no3.append(float(record[2]))
                    xsnintpjbrno3_no3=np.interp(wvltrue, wvlabsjbrno3_no3, xsnjbrno3_no3)
                    qyintpjbrno3_no3=np.interp(wvltrue, wvlabsjbrno3_no3, qyjbrno3_no3)
                    
    
                wvlabsjbrno3_no3array=np.array(wvlabsjbrno3_no3, float)
                xsnjbrno3_no3array=np.array(xsnjbrno3_no3, float)       

            with open ("absqyBrNO2_v2_IUPAC.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjbrno2.append(float(record[0]))
                    xsnjbrno2.append(float(record[1]))
                    qyjbrno2.append(float(record[2]))
                    xsnintpjbrno2=np.interp(wvltrue, wvlabsjbrno2, xsnjbrno2)
                    qyintpjbrno2=np.interp(wvltrue, wvlabsjbrno2, qyjbrno2)
                    
    
                wvlabsjbrno2array=np.array(wvlabsjbrno2, float)
                xsnjbrno2array=np.array(xsnjbrno2, float)    

            with open ("absqyHOI_Sander.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjhoi.append(float(record[0]))
                    xsnjhoi.append(float(record[1]))
                    qyjhoi.append(float(record[2]))
                    xsnintpjhoi=np.interp(wvltrue, wvlabsjhoi, xsnjhoi)
                    qyintpjhoi=np.interp(wvltrue, wvlabsjhoi, qyjhoi)
                    
    
                wvlabsjhoiarray=np.array(wvlabsjhoi, float)
                xsnjhoiarray=np.array(xsnjhoi, float)    

            with open ("absqyIO_Sander.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjio.append(float(record[0]))
                    xsnjio.append(float(record[1]))
                    qyjio.append(float(record[2]))
                    xsnintpjio=np.interp(wvltrue, wvlabsjio, xsnjio)
                    qyintpjio=np.interp(wvltrue, wvlabsjio, qyjio)
                    
    
                wvlabsjioarray=np.array(wvlabsjio, float)
                xsnjioarray=np.array(xsnjio, float)    

            with open ("absqyOIO_Sander.txt", "rt") as infile:
                for n in range(1):
                    infile.readline()
    
    
                for line in infile:
                    record=line.split()
                    wvlabsjoio.append(float(record[0]))
                    xsnjoio.append(float(record[1]))
                    qyjoio.append(float(record[2]))
                    xsnintpjoio=np.interp(wvltrue, wvlabsjoio, xsnjoio)
                    qyintpjoio=np.interp(wvltrue, wvlabsjoio, qyjoio)
                    
    
                wvlabsjoioarray=np.array(wvlabsjoio, float)
                xsnjoioarray=np.array(xsnjoio, float)    

                #idarray=np.array(Idmod, float)
                #idoubarray=np.array(Idoub, float)
                #iddoubarray=np.array(Iddoub, float)
                
    #            qyjnoaarray=np.array(qyjnoa, float)
    #            x=wvlabsjnoaarray[-5:]
    #            y=xsnjnoaarray[-5:]
    #            def func_powerlaw(x, m, c0):
    #                return c0 + x*m
    #            target_func = func_powerlaw
    #
    #
    #            popt, pcov = curve_fit(target_func, x, y)                
    #            plt.figure(figsize=(10, 5))
    #            plt.plot(x, target_func(x, *popt), '--')
    #            plt.plot(x, y, 'ro')
    #            plt.show()   
                    
                    
                    
                    
                    
    #            flux = ((iarray-float(I0[1]))/darray)*5.5
    #                wvlval=np.arange(287, 757, 0.1)
    #           fluxintp=np.interp(wvl, wvl, flux)
    #                wvlval2=np.arange(280, 750, 0.1)
                #iarray=np.array(Imod, float)
                #idarray=np.array(Idmod, float)
                #isarray=iarray-idarray
                #idoubarray=np.array(Idoub, float)
                #iddoubarray=np.array(Iddoub, float)
                darray=np.array(D, float)
                warray=np.array(Wmod, float)
                x=warray[:10]
                y=isarray[:10]
                def func_powerlaw(x, m, c0):
                    return c0 + x*m
                target_func = func_powerlaw
    
    
                popt, pcov = curve_fit(target_func, x, y)
    
               #plt.figure(figsize=(10, 5))
                #plt.plot(x, target_func(x, *popt), '--')
                #plt.plot(x, y, 'ro')
                #plt.show()
    
    
    
                
    #            z = np.powfit(x, y, 2)
    #            p = np.poly1d(z)
    #            p30 = np.poly1d(np.polyfit(x, y, 30))
    #            xp = np.linspace(280, 800, 100)
    #            _ = plt.plot(x, y, '.', xp, p(xp), '-')
    #            plt.ylim(2460,24800)
    #            (2460, 24800)
    #            plt.show()
                parray=np.array(popt, float)
                carray=np.array(pcov, float)
                #print(popt)
    #            print(parray[0], parray[1], parray[2])
                isarray=isarray-(warray*parray[0]+parray[1])
                np.seterr(divide='ignore',invalid='ignore')
                itrue_dmatch=isarray[10:]
                flux=(isarray*1)/(darray/((0.1/float(s[3]))))
                #plt.plot(wvltrue,flux)
                #flux = ((((Itrue-(float(I0[1])))-(idoubarray*0.00045))/darray))*1.25
                #flux = ((iarray-idarray)-((idoubarray-iddoubarray)*((2077.8*wvltrue**-2.582))))/(darray/(100000/float(s[3])))
                #flux = ((iarray)-((idoubarray)*((2077.8*wvltrue**-2.582))))/(darray/1.54)
    #                qyintp=np.interp(wvlval2, wvlabs, qy)
    #                qyintp=np.interp(wvlval2, wvlabs, qy)
                #18.891
               #splice=18.891+7.0785E-05*zennow**2.7184
                splice=39+7.0785E-05*zennow**2.7184
                jo1dwvl=flux*xsnintp*qyintp
                jo1dwvlsplice=jo1dwvl[int(splice):]
                jo1d=np.sum(jo1dwvlsplice)
                jno2wvl=flux*xsnintpjno2*qyintpjno2
                jno2wvlsplice=jno2wvl[int(splice):]
                jno2=np.sum(jno2wvlsplice)
                jhonowvl=flux*xsnintpjhono*qyintpjhono
                jhonowvlsplice=jhonowvl[int(splice):]
                jhono=np.sum(jhonowvlsplice)
                jhchorwvl=flux*xsnintpjhchor*qyintpjhchor
                jhchorwvlsplice=jhchorwvl[int(splice):]
                jhchor=np.sum(jhchorwvlsplice)
                jhchonrwvl=flux*xsnintpjhchonr*qyintpjhchonr
                jhchonrwvlsplice=jhchonrwvl[int(splice):]
                jhchonr=np.sum(jhchonrwvlsplice)
                jno3tonowvl=flux*xsnintpjno3tono*qyintpjno3tono
                jno3tonowvlsplice=jno3tonowvl[int(splice):]
                jno3tono=np.sum(jno3tonowvlsplice)
                jno3tono2wvl=flux*xsnintpjno3tono2*qyintpjno3tono2
                jno3tono2wvlsplice=jno3tono2wvl[int(splice):]
                jno3tono2=np.sum(jno3tono2wvlsplice)
                jch3chowvl=flux*xsnintpjch3cho*qyintpjch3cho
                jch3chowvlsplice=jch3chowvl[int(splice):]
                jch3cho=np.sum(jch3chowvlsplice)
                jch3coch3wvl=flux*xsnintpjch3coch3*qyintpjch3coch3
                jch3coch3wvlsplice=jch3coch3wvl[int(splice):]
                jch3coch3=np.sum(jch3coch3wvlsplice)
                jclno2wvl=flux*xsnintpjclno2*qyintpjclno2
                jclno2wvlsplice=jclno2wvl[int(splice):]
                jclno2=np.sum(jclno2wvlsplice)
                jclono2awvl=flux*xsnintpjclono2a*qyintpjclono2a
                jclono2awvlsplice=jclono2awvl[int(splice):]
                jclono2a=np.sum(jclono2awvlsplice)
                jclono2bwvl=flux*xsnintpjclono2b*qyintpjclono2b
                jclono2bwvlsplice=jclono2bwvl[int(splice):]
                jclono2b=np.sum(jclono2bwvlsplice)
                jhoclwvl=flux*xsnintpjhocl*qyintpjhocl
                jhoclwvlsplice=jhoclwvl[int(splice):]
                jhocl=np.sum(jhoclwvlsplice)
                jh2o2wvl=flux*xsnintpjh2o2*qyintpjh2o2
                jh2o2wvlsplice=jh2o2wvl[int(splice):]
                jh2o2=np.sum(jh2o2wvlsplice)
                jhno3wvl=flux*xsnintpjhno3*qyintpjhno3
                jhno3wvlsplice=jhno3wvl[int(splice):]
                jhno3=np.sum(jhno3wvlsplice)
                jc2h5chowvl=flux*xsnintpjc2h5cho*qyintpjc2h5cho
                jc2h5chowvlsplice=jc2h5chowvl[int(splice):]
                jc2h5cho=np.sum(jc2h5chowvlsplice)
                j15c3h7chowvl=flux*xsnintpj15c3h7cho*qyintpj15c3h7cho
                j15c3h7chowvlsplice=j15c3h7chowvl[int(splice):]
                j15c3h7cho=np.sum(j15c3h7chowvlsplice)
                j16c3h7chowvl=flux*xsnintpj16c3h7cho*qyintpj16c3h7cho
                j16c3h7chowvlsplice=j16c3h7chowvl[int(splice):]
                j16c3h7cho=np.sum(j16c3h7chowvlsplice)
                jiprchowvl=flux*xsnintpjiprcho*qyintpjiprcho
                jiprchowvlsplice=jiprchowvl[int(splice):]
                jiprcho=np.sum(jiprchowvlsplice)
                j18macrwvl=flux*xsnintpj18macr*qyintpj18macr
                j18macrwvlsplice=j18macrwvl[int(splice):]
                j18macr=np.sum(j18macrwvlsplice)
                j19macrwvl=flux*xsnintpj19macr*qyintpj19macr
                j19macrwvlsplice=j19macrwvl[int(splice):]
                j19macr=np.sum(j19macrwvlsplice)
                jhpaldwvl=flux*xsnintpjhpald*qyintpjhpald
                jhpaldwvlsplice=jhpaldwvl[int(splice):]
                jhpald=np.sum(jhpaldwvlsplice)
                jmekwvl=flux*xsnintpjmek*qyintpjmek
                jmekwvlsplice=jmekwvl[int(splice):]
                jmek=np.sum(jmekwvlsplice)
                j23mvkwvl=flux*xsnintpj23mvk*qyintpj23mvk
                j23mvkwvlsplice=j23mvkwvl[int(splice):]
                j23mvk=np.sum(j23mvkwvlsplice)
                j24mvkwvl=flux*xsnintpj24mvk*qyintpj24mvk
                j24mvkwvlsplice=j24mvkwvl[int(splice):]
                j24mvk=np.sum(j24mvkwvlsplice)
                jmglyoxwvl=flux*xsnintpjmglyox*qyintpjmglyox
                jmglyoxwvlsplice=jmglyoxwvl[int(splice):]
                jmglyox=np.sum(jmglyoxwvlsplice)
                jbiacetwvl=flux*xsnintpjbiacet*qyintpjbiacet
                jbiacetwvlsplice=jbiacetwvl[int(splice):]
                jbiacet=np.sum(jbiacetwvlsplice)
                jch3oohwvl=flux*xsnintpjch3ooh*qyintpjch3ooh
                jch3oohwvlsplice=jch3oohwvl[int(splice):]
                jch3ooh=np.sum(jch3oohwvlsplice)
                jch3no3wvl=flux*xsnintpjch3no3*qyintpjch3no3
                jch3no3wvlsplice=jch3no3wvl[int(splice):]
                jch3no3=np.sum(jch3no3wvlsplice)
                jc2h5no3wvl=flux*xsnintpjc2h5no3*qyintpjc2h5no3
                jc2h5no3wvlsplice=jc2h5no3wvl[int(splice):]
                jc2h5no3=np.sum(jc2h5no3wvlsplice)
                jnc3h7no3wvl=flux*xsnintpjnc3h7no3*qyintpjnc3h7no3
                jnc3h7no3wvlsplice=jnc3h7no3wvl[int(splice):]
                jnc3h7no3=np.sum(jnc3h7no3wvlsplice)
                jic3h7no3wvl=flux*xsnintpjic3h7no3*qyintpjic3h7no3
                jic3h7no3wvlsplice=jic3h7no3wvl[int(splice):]
                jic3h7no3=np.sum(jic3h7no3wvlsplice)
                jtc4h9no3wvl=flux*xsnintpjtc4h9no3*qyintpjtc4h9no3
                jtc4h9no3wvlsplice=jtc4h9no3wvl[int(splice):]
                jtc4h9no3=np.sum(jtc4h9no3wvlsplice)
                jnoawvl=flux*xsnintpjnoa*qyintpjnoa
                jnoawvlsplice=jnoawvl[int(splice):]
                jnoa=np.sum(jnoawvlsplice)
                j31glyoxwvl=flux*xsnintpj31glyox*qyintpj31glyox
                j31glyoxwvlsplice=j31glyoxwvl[int(splice):]
                j31glyox=np.sum(j31glyoxwvlsplice)
                j32glyoxwvl=flux*xsnintpj32glyox*qyintpj32glyox
                j32glyoxwvlsplice=j32glyoxwvl[int(splice):]
                j32glyox=np.sum(j32glyoxwvlsplice)
                j33glyoxwvl=flux*xsnintpj33glyox*qyintpj33glyox
                j33glyoxwvlsplice=j33glyoxwvl[int(splice):]
                j33glyox=np.sum(j33glyoxwvlsplice)
                j2o3wvl=flux*xsnintpj2o3*qyintpj2o3
                j2o3wvlsplice=j2o3wvl[int(splice):]
                j2o3=np.sum(j2o3wvlsplice)
                jch3iwvl=flux*xsnintpjch3i*qyintpjch3i
                jch3iwvlsplice=jch3iwvl[int(splice):]
                jch3i=np.sum(jch3iwvlsplice)
                jbrowvl=flux*xsnintpjbro*qyintpjbro
                jbrowvlsplice=jbrowvl[int(splice):]
                jbro=np.sum(jbrowvlsplice)  
                jhobrwvl=flux*xsnintpjhobr*qyintpjhobr
                jhobrwvlsplice=jhobrwvl[int(splice):]
                jhobr=np.sum(jhobrwvlsplice)  
                jbrno3_no2wvl=flux*xsnintpjbrno3_no2*qyintpjbrno3_no2
                jbrno3_no2wvlsplice=jbrno3_no2wvl[int(splice):]
                jbrno3_no2=np.sum(jbrno3_no2wvlsplice)              
                jbrno3_no3wvl=flux*xsnintpjbrno3_no3*qyintpjbrno3_no3
                jbrno3_no3wvlsplice=jbrno3_no3wvl[int(splice):]
                jbrno3_no3=np.sum(jbrno3_no3wvlsplice)            
                jbrno2wvl=flux*xsnintpjbrno2*qyintpjbrno2
                jbrno2wvlsplice=jbrno2wvl[int(splice):]
                jbrno2=np.sum(jbrno2wvlsplice) 
                jhoiwvl=flux*xsnintpjhoi*qyintpjhoi
                jhoiwvlsplice=jhoiwvl[int(splice):]
                jhoi=np.sum(jhoiwvlsplice) 
                jiowvl=flux*xsnintpjio*qyintpjio
                jiowvlsplice=jiowvl[int(splice):]
                jio=np.sum(jiowvlsplice)               
                joiowvl=flux*xsnintpjoio*qyintpjoio
                joiowvlsplice=joiowvl[int(splice):]
                joio=np.sum(joiowvlsplice)             
                yr=t[6]
    #
    #
            with open("CV_2023-08_39s_R_CH3I_HALOGENS.txt", "at") as outfile:
                # print(t[3]+" "+t[2]+" "+yr[:4]+" "+t[4]+","+str(jo1d)+","+str(j2o3)+","+str(jh2o2)+","+str(jno2)+","+str(jno3tono)+","+str(jno3tono2)+","+str(jhono)+","+str(jhno3)+","+str(jhchor)+","+str(jhchonr)+","+str(jch3cho)+","+str(jc2h5cho)+","+str(j15c3h7cho)+","+str(j16c3h7cho)+","+str(jiprcho)+","+str(j18macr)+","+str(j19macr)+","+str(jhpald)+","+str(jch3coch3)+","+str(jmek)+","+str(j23mvk)+","+str(j24mvk)+","+str(j31glyox)+","+str(j32glyox)+","+str(j33glyox)+","+str(jmglyox)+","+str(jbiacet)+","+str(jch3ooh)+","+str(jch3no3)+","+str(jc2h5no3)+","+str(jnc3h7no3)+","+str(jic3h7no3)+","+str(jtc4h9no3)+","+str(jnoa)+","+str(jclno2)+","+str(jclono2a)+","+str(jclono2b)+","+str(jhocl)+","+str(jch3i)+","+str(jbro)+","+str(jhobr)+","+str(jbrno3_no2)+","+str(jbrno3_no3)+","+str(jbrno2)+","+str(jhoi)+","+str(jio)+","+str(joio)+","+str(popt[0])+","+str(pcov[0,0]**0.5)+","+str(popt[1])+","+str(pcov[1,1]**0.5)+","+str(zennow), file=outfile)
                 print(t[3]+" "+t[2]+" "+yr[:4]+" "+t[4]+","+str(jo1d)+","+str(j2o3)+","+str(jh2o2)+","+str(jno2)+","+str(jno3tono)+","+str(jno3tono2)+","+str(jhono)+","+str(jhno3)+","+str(jhchor)+","+str(jhchonr)+","+str(jch3cho)+","+str(jc2h5cho)+","+str(j15c3h7cho)+","+str(j16c3h7cho)+","+str(jiprcho)+","+str(j18macr)+","+str(j19macr)+","+str(jhpald)+","+str(jch3coch3)+","+str(jmek)+","+str(j23mvk)+","+str(j24mvk)+","+str(j31glyox)+","+str(j32glyox)+","+str(j33glyox)+","+str(jmglyox)+","+str(jbiacet)+","+str(jch3ooh)+","+str(jch3no3)+","+str(jc2h5no3)+","+str(jnc3h7no3)+","+str(jic3h7no3)+","+str(jtc4h9no3)+","+str(jnoa)+","+str(jclno2)+","+str(jclono2a)+","+str(jclono2b)+","+str(jhocl)+","+str(jch3i)+","+str(jbro)+","+str(jhobr)+","+str(jbrno3_no2)+","+str(jbrno3_no3)+","+str(jbrno2)+","+str(jhoi)+","+str(jio)+","+str(joio)+","+str(popt[0])+","+str(pcov[0,0]**0.5)+","+str(popt[1])+","+str(pcov[1,1]**0.5)+","+str(zennow), file=outfile)
