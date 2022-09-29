%sub_group A:

pd = makedist('Normal','mu',3.5,'sigma',0.25);
tpd=truncate(pd,1,10);


k=10;
t_betas=random(tpd,1,k);
histogram(t_betas);
%save('true_sc1_betas.mat','t_betas');

%sub-group B:

pd = makedist('Normal','mu',2,'sigma',0.15);
tpd=truncate(pd,1,10);


k=10;
t_betas=random(tpd,1,k);
histogram(t_betas);
%save('true_sc2_betas.mat','t_betas');

%sub-group C:

pd = makedist('Normal','mu',2.5,'sigma',0.5);
tpd=truncate(pd,1,10);


k=10;
t_betas=random(tpd,1,k);
histogram(t_betas);
%save('true_sc3_betas.mat','t_betas');
