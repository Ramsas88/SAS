
libname sdtm ""; /*need to give sas datasets path*/ 
%let saspath=""; /*need to give sas datasets path*/
%let xptpath=""; /*need to give xpt dataset path*/

/********************CREATE MACRO VARIABLES FOR DATASETS***********************************/
proc sql noprint;
    create table dsets as 
   select distinct memname from sashelp.vcolumn
   where libname eq "SDTM";
   select count(memname) into : cnt from dsets;
   select memname into : dsets separated by "@" from dsets;
quit;

/*******************SAS DATASETS TO XTP CONVERSION*******************************************/
%macro sas2xpt;
%do i=1 %to &cnt.;
%let dset=%scan(&dsets,&i,"@");

libname sasfile "&saspath.";  *this is where the SAS data set reside;
libname xptfile xport "&xptpth.\&dset..xpt";  *this is where the XPORT file will be created;
proc copy in=sasfile out=xptfile memtype=data;
   select &dset.;
run;
%end;
%mend;
/********************************MACRO CALL********************************************************/
%sas2xpt;
