modelexplorer <-function(runid, featurelistforNN,outputdirectory)
{
#  NNperformancechart <<-1000
  runid = runid
  featurelistforNN = featurelistforNN
#rm(list=ls()) 

if(!exists("modeltrainer", mode="function")) source("./recomendationsystems/1_modeltrainer_FCNN4R.R")
#this line to pull in the data and structure it
if(!exists("Combinestocks", mode="function")) source("./datasetcreation/Combinestocks.R")
#this line to construct the training/outputsets
if(!exists("modelperformance", mode="function")) source("./recomendationsystems/modelperformance.R")

#trainmodel<- function(epocs,netdepth,layer2,layer3,layer4,tol_level,learn_rate,l2reg) 

#netdepth
#layer2
#layer3
#layer4
#tol_level
#learn_rate
#learnratelist=c(.1,.01,.02,.03,.04,0.05,.06,.07,.08,.09,.001,.002,.003,.004,.005,.006,.007,.008,.009)
#l2reg

#TODO:
#to Start over from the last place you were at uncomment this line
#lastresults= tail(read.csv('results.csv'),1)  # read csv file

#lastrunresults=tail(read.csv('results.csv'),1)

layer3=0
layer4=0
#depth


#One time run uncomment and modify  run everything including the  next 2 line and above
report_freq <<- 20
numberofstockstouse=length(featurelistforNN)
#sgd specific parameters
minibatchsz = 100
lambda = .1
gamma = .01
momentum = .7

#combinestocksfunction(800)
#trainmodel(400,5,1600,1600,1600,.05,.01,.05)
#remove any stocks that couldn't get data for and create the base stock dataset
numberofstockstouse = combinestocksfunction(numberofstockstouse, featurelistforNN, outputdirectory)
#cat("numberofstockstouse:", numberofstockstouse, "\n" )

#For easy reference
#trainmodel <- function(runid,              numberofstockstouse,minibatchszparam,lambdaparam,gammaparam,momentumparam,epocsparam,netdepthparam,layer2param,layer3param,layer4param,tol_levelparam,learn_rateparam,l2regparam)
modelperformance =  trainmodel(runid, numberofstockstouse,      minibatchsz,     lambda,     gamma,     momentum,     10,        3,            1000,        400,        200,        .05,           .01,            .03, outputdirectory)


#for (epocs in (c(500))){
#  for (depth in 5){
#    if (depth==3){
#      for (layer2 in (seq(210, 300, by=30))){
#            for (learnrate in (c(.1,.01,.02,.03,.04,0.05,.06,.07,.08,.09,.001,.002,.003,.004,.005,.006,.007,.008,.009))){
#              for (l2reg in (c(.1,.2,.3,.4,.5,.6,.7,.8,.9))){
#                trainmodel(epocs,as.integer(depth),layer2,layer3,layer4,.05,learnrate,l2reg)
#              }}}}
#    if (depth==4){    
#      for (layer2 in (seq(60, 300, by=30))){
#        for (layer3 in (seq(60, 300, by=30))){
#            for (learnrate in (c(.1,.01,.02,.03,.04,0.05,.06,.07,.08,.09,.001,.002,.003,.004,.005,.006,.007,.008,.009))){
#              for (l2reg in (c(.1,.2,.3,.4,.5,.6,.7,.8,.9))){
#                trainmodel(epocs,as.integer(depth),layer2,layer3,layer4,.05,learnrate,l2reg)
#              }}}}}
#            
#    if (depth==5){    
#      for (layer2 in (seq(200, 300, by=50))){
#        for (layer3 in (seq(200, 300, by=50))){
#          for (layer4 in (seq(200, 300, by=50))){
#            for (learnrate in (c(.01,.05,.1,.001,.005))){
#              for (l2reg in (c(.01,.05,.1,.001,.005))){
#                for(numberofstockstouse in (c(seq(100,250, by=50)))){
#                combinestocksfunction(numberofstockstouse)
#                print("combiningstocksexited")
#                trainmodel(epocs,as.integer(depth),layer2,layer3,layer4,.05,learnrate,l2reg)
#                }
#                
#              }}}}}}
#}}
#    

#plot(NNperformancechart)
return(modelperformance)
}
