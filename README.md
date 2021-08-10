# Empatica-E4-Data-Processing
Empatica E4 Data Processing: HRV, EDA, movement, inter-beat intervals

Notes for running the code: 
Plotly requires updated R and RStudio versions 
install.packages(“package-name”) in console if missing a package prior to running
Change line 14 to appropriate path of folder containing zip files
Output files code found on lines 142-143.

Participant 11 & 18 from Aim 1 of the Glenbeigh Study
Pre-processing:
Removed artifacts from IBI
Identified “runs” of uninterrupted data
Per Malik (1996) the standard for RMSSD measurements are 5 minute intervals. However, according to Shaffer & Ginsberg (2017) shorter intervals are permissible. I opted for 3 minute time windows so as to have enough data to visualize.
Calculated the RMSSD from IBI (see Shaffer & Ginsberg for formula). 
Condensed to 3rds of the day with 1 hour breaks between analysis periods to include spacing in visuals.
Period 1: Sleep After 12AM and before  7 AM
Period 2: Morning & Mid-day: After 8AM and before 3PM
Period 3: Afternoon & Evening: After 4PM and before 11PM
By day, by analysis window extract the HRV measurement with the lowest movement (in g-force). 
Columns
IBI/diff = Interbeat interval
HRV = Heart rate variability in RMSSD
RMSSD = Root mean square of successive difference, interchangeable with HRV in seconds.
AVG_RMSSD = Mean RMSSD over window period (in condensed data.)
Rlid = Factor denoting id of analysis window.
Count = Count of reading within analysis window
Window size = Length of the window for analysis in seconds
ACC = Instantaneous G-force which is sqrt of sum of X acceleration squared, Y acceleration squared, and Z acceleration squared,
Movement = Average ACC over window period (in condensed data.)
Hour of day in military time
Day of dataset

References:
Malik, M. (1996). Heart rate variability: Standards of measurement, physiological interpretation, and clinical use: Task force of the European Society of Cardiology and the North American Society for Pacing and Electrophysiology. Annals of Noninvasive Electrocardiology, 1(2), 151-181.
Shaffer, F., & Ginsberg, J. P. (2017). An overview of heart rate variability metrics and norms. Frontiers in public health, 5, 258.
