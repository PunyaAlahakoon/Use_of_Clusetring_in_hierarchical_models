%Dataset 1:

pd = makedist('Normal','mu',3.5,'sigma',0.25);
tpd=truncate(pd,1,10);

from the genrated values:

mean=   3.4767
std=   0.3386


#DAteset 2
pd = makedist('Normal','mu',2,'sigma',0.15);
tpd=truncate(pd,1,10);


mean=  1.9426
std=  0.1921


%Dataset 3:
pd = makedist('Normal','mu',2.5,'sigma',0.5);
tpd=truncate(pd,1,10);

mean=   2.6737
std=   0.4136



#############all three:
mean=  2.6977

std=    0.7115


################datsets 1 and 2:
mean=    2.7096
std=  0.8313

##################################################
gammas:
all three datasets:

pd = makedist('Normal','mu',1,'sigma',0.05);
tpd=truncate(pd,0,4);

mean=  1.0104
std= 0.0511


#################################################
epsilons:
all three datasets:
pd = makedist('Normal','mu',0.06,'sigma',0.01);
tpd=truncate(pd,0.04,4);


mean=  0.0586

std=   0.0093



