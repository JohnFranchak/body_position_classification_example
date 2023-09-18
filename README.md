This repository contains the data and code in support of the manuscript [Long-form recording of infant body position in the home using wearable inertial sensors](https://doi.org/10.3758/s13428-023-02236-9). This repository contains all of the data required to synchronize IMUs to video annotations for a single participant and create windowed motion features. The video files associated with this project are available to authorized users on Databrary (https://nyu.databrary.org/volume/1580). Note, for computational efficiency and file storage reasons, we have truncated the sensor data so that it only includes times that correspond to when videos were available. The full data set extends across the entire day.

The folder "imu" contains acceleration and gyroscope data for each of the four sensors and "study_events.csv" which contains the time of the synchronization point in the IMU data. The folder "video" contains the Datavyu annotation files (video coding). "Activity.csv" is exported from the part 1 video using the export_pos.rb script in the "script" folder. Note that the synchronization point is coded from video and used in the script to adjust all coded times to be relative to the synchronization point as a time origin. 

Please cite the publication of record if you use data or code in your own project. The best current citation for this work is:

Franchak, J. M., Tang, M., Rousey, H., & Luo, C. (2023). Long-form recording of infant body position in the home using wearable inertial sensors. _Behavior Research Methods_. https://doi.org/10.3758/s13428-023-02236-9.


