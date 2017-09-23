#!/bin/bash


# Batch file to launch the loop over all scenarios

cd ..

# create output file if it does not exist
mkdir -p outputs

# clear previous time trackers
rm -f outputs/time.txt


#################################

# # SIMPLE LOOP

# for dem in EFF
# do

# 	for enr in low #low or high
# 	do
# 		for retro in 40 # 40 to 90
# 		do
# 			for new in high # low, medium or high
# 			do
# 				for CO2 in low medium high #low, medium or high
# 				do
					
# 					if [ -e outputs/model_${dem}_${enr}enr_${retro}ret_${new}new_${CO2}CO2.lst ]
# 					then
# 					    echo "run already made"

# 					else

# 					    # create of file to indicate the run is started (to avoid double calculations during multiple simultaneous runs)
# 					    echo running >> outputs/model_${dem}_${enr}enr_${retro}ret_${new}new_${CO2}CO2.lst

# 						echo "starting run with demand ${dem}, retrofit ${retro}, new nuc ${new} and CO2 ${CO2}" >> outputs/time.txt
# 						echo "starting run with demand ${dem}, retrofit ${retro}, new nuc ${new} and CO2 ${CO2}" 
# 						start_time=`date +%s`

# 						cd code
# 						gams model.gms --demand=${dem} --enr=$enr --retro=$retro --new=$new --CO2=$CO2
# 						cd ..
# 						mv code/model.lst outputs/model_${dem}_${enr}enr_${retro}ret_${new}new_${CO2}CO2.lst

# 						end_time=`date +%s`
# 						echo execution time was `expr $end_time - $start_time` s. >> outputs/time.txt
# 					fi



# 				done

# 			done

# 		done

# 	done

# done



#################################

FULL LOOP

for dem in SOB DIV DEC EFF
do

	for enr in low high #low or high
	do
		for retro in 40 50 60 70 80 90 # 40 to 90
		do
			for new in high medium low # low, medium or high
			do
				for CO2 in low medium high #low, medium or high
				do
					
					if [ -e outputs/model_${dem}_${enr}enr_${retro}ret_${new}new_${CO2}CO2.lst ]
					then
					    echo "run already made"

					else

					    # create of file to indicate the run is started (to avoid double calculations during multiple simultaneous runs)
					    echo running >> outputs/model_${dem}_${enr}enr_${retro}ret_${new}new_${CO2}CO2.lst

						echo "starting run with demand ${dem}, retrofit ${retro}, new nuc ${new} and CO2 ${CO2}" >> outputs/time.txt
						echo "starting run with demand ${dem}, retrofit ${retro}, new nuc ${new} and CO2 ${CO2}" 
						start_time=`date +%s`

						cd code
						gams model.gms --demand=${dem} --enr=$enr --retro=$retro --new=$new --CO2=$CO2
						cd ..
						mv code/model.lst outputs/model_${dem}_${enr}enr_${retro}ret_${new}new_${CO2}CO2.lst

						end_time=`date +%s`
						echo execution time was `expr $end_time - $start_time` s. >> outputs/time.txt
					fi



				done

			done

		done

	done

done

