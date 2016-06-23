#!/bin/bash

for filename in /Volumes/AOP-NEON1-4/D17/SOAP/2013/SOAP_L1/SOAP_Lidar/Classified_point_cloud/*.laz
do 
	echo "Uncompressing " $filename
	las2las -i $filename -o $filename.las
	
done 




