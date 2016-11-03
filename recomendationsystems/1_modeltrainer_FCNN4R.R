
#using FCNN4R for it's ability to copy NNs to each other and stack/run parrellel on the same data, and Pruning on the same set of data.
trainmodel <- function(runid, numberofstockstouse,minibatchszparam,lambdaparam,gammaparam,momentumparam,epocsparam,netdepthparam,layer2param,layer3param,layer4param,tol_levelparam,learn_rateparam,l2regparam) {
#print("Enteringtrainmodel----------")
#cat("NETDEPTH:", netdepthparam, "\n")


library(FCNN4R)
inputlayersize <<- as.double(diminputpercentagematrix[2])
#cat("INPUTLAYERSIZE", inputlayersize, "\n")

#Variablize the NN Parameters  #SA is preferred due to its quick descent, but obj_func needs fixed before it can be re-enabled.
nettype = 'sa' # 'sgd' or 'bp' or 'sa' simulated annealing stochastic gradient descent or Back propogation 

netdepth<<-netdepthparam
#print(netdepth)
inputlayer <<- inputlayersize
layer2 <<- layer2param
layer3 <<- layer3param
layer4 <<- layer4param
ouputlayer <<- numberofstockstouse

#BPparameters
tol_level <<- tol_levelparam
learn_rate <<- learn_rateparam
max_epochs <<- epocsparam
l2reg <<- l2regparam

#sgd specific parameters
#minibatchsz <<- 100
#lambda = .1
#gamma = .01
#momentum = .7
minibatchsz = minibatchszparam
lambda = lambdaparam
gamma = gammaparam
momentum = momentumparam

#other
u = 1.2 
d = 0.5 
gmax = 50 
gmin = 1e-06
report_freq <<- 10
input <<- as.matrix(percentchangedcombined_train[,1:inputlayersize])

output <<- as.matrix(percentchangedcombined_train[,(inputlayersize+1):(dim(percentchangedcombined)[2])])
#cat("Output Dims", dim(output), "\n")
ouputlayer = dim(output)[2]

evalmatrix <<- as.matrix(percentchangedcombined_eval)
slope = .3

#activation character string, activation function name, admissible options are: "threshold", "sym_threshold", "linear", "sigmoid", "sym_sigmoid" (and "tanh"), "sigmoid_approx", and "sym_sigmoid_approx"
hidden_activation_function = "sigmoid_approx"
output_activation_function = "sigmoid_approx" #(needs to be linear for regression)

#hidden later numbers are inbedded in next line and instanciating the net
if (netdepth==5){
  print("netdepth Is 5")
  print(paste("layer2count: ", layer2,sep=''))
  print(paste("layer3count: ", layer3,sep=''))
  print(paste("layer4count: ", layer4,sep=''))
  mymlpnet <<- mlp_net(c(inputlayer,layer2,layer3,layer4,ouputlayer))
}

if (netdepth==4){
  print("Netdepth Is 4")
  print(paste("layer2count: ", layer2,sep=''))
  print(paste("layer3count: ", layer3,sep=''))
  mymlpnet <<- mlp_net(c(inputlayer,layer2,layer3,ouputlayer))
  layer4=0
}

if (netdepth==3){
#  print("Netdepth Is 3")
#  print(paste("layer2count: ", layer2,sep=''))
  mymlpnet <<- mlp_net(c(inputlayer,layer2,ouputlayer))
  layer4=0
  layer3=0
}

#set activation function on the newly created net
#mymlpnet = mlp_set_activation(mymlpnet, layer = "a", activation = hidden_activation_function, slope = slope)
mymlpnet <<- mlp_set_activation(mymlpnet, layer = "h", activation = hidden_activation_function, slope = slope)
mymlpnet <<- mlp_set_activation(mymlpnet, layer = "o", activation = output_activation_function, slope = slope)

#randomize the weights
mymlpnet <<- mlp_rnd_weights(mymlpnet)
#train the net
if (nettype=='bp'){
  mymlpnet_trained <<- mlp_teach_bp(mymlpnet, input, output, tol_level, max_epochs, learn_rate, l2reg, report_freq)
}
if (nettype=='sgd'){
  print
  mymlpnet_trained <<- mlp_teach_sgd(mymlpnet, input, output, tol_level, max_epochs, learn_rate, l2reg, minibatchsz, lambda, gamma, momentum, report_freq)
}
if (nettype=='sa'){
  #This needs fixed for SA to work....
  timerstart = 0
  timer = timerstart
  obj_func <- function(net)
  {
    #This timer is to keep the trainer from getting into some sort of loop.  There must be a bug somewhere in the mlp_mse function
    timer <<- timer+1
    if(timer > timerstart+50){return (-10000)}
    #cat(timer, "IN obj_func, breaking at 50\n", sep = ':')
    return(mlp_mse(mymlpnet, input, output))
  }
  for(i in 1){
  mymlpnet_trained <<- mlp_teach_sa(mymlpnet, obj_func, Tinit = 1, max_epochs, report_freq, report_action = NULL)
  }
}
cat("MyMlpNet Trained\n")

#clean the net
mymlpnet_clean <<- mlp_rm_input_neurons(mymlpnet_trained$net, report = TRUE)

mlpeval_eval <<- mlp_eval(mymlpnet_clean$net,evalmatrix[,1:inputlayersize])
cat("MyMlpNet Evaluated\n")
thismodelsperformance=modelperformance(mlpeval_eval)
print(paste("Performance: ", thismodelsperformance, sep = ''))
#write results to the results file.
thisrun=paste(runid, nettype,max_epochs,netdepth,inputlayer,layer2,layer3,layer4,ouputlayer,tol_level,max_epochs,learn_rate,l2reg,u,d,gmax,gmin,report_freq,slope,hidden_activation_function,output_activation_function,minibatchsz,lambda,gamma,momentum,tail(mymlpnet_trained$mse,1),thismodelsperformance,sep = ',')
write(thisrun,file="data/results.csv",append=TRUE)
return(thismodelsperformance)
}