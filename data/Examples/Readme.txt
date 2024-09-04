There are few examples to introduce the usage of GRAVS2. For better understanding 
of the processing logic, command lines, options and parameters chosen, it is 
recommended to follow the manual as well.

1) 1st example in folder "2010_Calibration" indroduces the evaluation of 
relative gravimeter's scale for calibration. 

   There are two campaigns:
   
   i) First campaign in "2010_Haanja-Toila_base_S36" connects Haanja, Toravere, 
      Toila absolute stations (Haanja-Toila calibration line with range 170 mGal)
	  in 2010 to check the scale of CG-5 type gravimeter #10036 (S-36) gravimeter.
	  All processing was made in same folder.
      
      Several adjustment steps were needed (*ADJ0, *ADJ1,... collected into zip 
      files) to find the best solution. ADJ0 was made with default input 
      parameters, without fixed stations (non-constrained adjustment). ADJ1 was 
      fixed by 1 station to find good drift and offset values and so on.
      
      Finally the scale was evaluated using fully constrained adjustment 
      (with all three fixed AG stations). It was found that scale change of S-36 
      is 0.976E-04 +/- 0.74E-05 (97.6 ± 7.5 ppm) with polynomial deg=1. Deg=99 
      gave consistent result: 0.99990238 +/- 0.00000749 -> 97.6 ± 7.5 ppm (see ADJ2).

   ii) Second campaign "2010_Parnu_base" in 2010 along Parnu calibration line 
       (connecting 5 RG stations with range 82 mGal) the S-36 and LCR G-191
	   observations were combined to evaluate the scale of G-191. The scale of
	   S36 was already checked in previous example and can be now used to check
	   the scale of G-191. It can be done in several ways. For instance, the 
	   adjustment results of S-36 data can be used to fix all 5 stations. 
	   Another way is the adjustment of 2 gravimeters together by fixing only 
	   1 station (Reiu, connected with nearest Audru AG station) and upscaling 
	   the weights of S-36 data (to fix the adjustment). The latter approach was
	   chosen to proceed and from the joint adjustment in folder \Adj_comb the 
	   scale change of LCR G-191 was found to be:
	   a) linear term (deg=1): 	673.9 ± 30.4 ppm
	   b) amplitude A and phase phi of periodic wave with period T=70.9412 C.U.
	      are 66.77 ± 0.88 uGal, 185.76° ± 0.94°, respectively.
	   
	   NB: Wave with T=35.4706 CU was also tested, but result was insignificant.
	   These scale change terms of G-191 agree quite well with values presented
	   by Oja et al (2011).


2) 2nd example "2010-03-17_GulfofRiga" is based on the data processing of gravity 
survey measured on ice of frozen Baltic sea, for details see Oja et al (2011).

The processing combines the measurements of two teams, one uses CG-5 gravimeter 
S-36, another LCR G-191. At first the data of both teams were processed separately
to find favourable control keys for drift, offsets (tares), skips based on 
repeatability. Note that calibration results from 1st example were also applied. 
Moreover, re-weighting helps to reduce the effect of unstable ice measurements on 
adjusted results, especially on drift modelling.

Joint adjustment in folder \adj_all gave the best solution with adjusted gravity
values by refining the values of control keys and stochastic model.


3) "Haanja_2008-2019_VGG" -- Evaluation of vertical gravity gradient (VGG) at Haanja AG station.
COMMANDS:
evalvgg Haanja_2008-2019.dat
evalvgg Haanja_2008-2019.dat 2


References

Oja, T., Türk, K., Ellmann, A., Gruno, A., Bloom, A., and Sulaoja, M. (2011). Relative
gravity surveys on ice-covered water bodies. In Proceedings of the International Con-
ference on Environmental Engineering. ICEE, volume 8, pages 1394–1401. Vilnius
Gediminas Technical University.
