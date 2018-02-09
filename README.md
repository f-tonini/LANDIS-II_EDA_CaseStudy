#Modeling Epidemiological Disturbances in LANDIS-II

This repository was created to host reproducible code, data, and installers for users interested in replicating the LANDIS-II simulation illustrated in the research manuscript:

Francesco Tonini, Chris Jones, Brian R. Miranda, Richard C. Cobb, Brian R. Sturtevant, Ross K. Meentemeyer. 2018. Modeling epidemiological disturbances in LANDIS-II. _Ecography_.

#Instructions

Please follow these steps for installation and running the model (__NOTE: needs to be used with Windows OS as it is written in C# using .NetFramework__):

1. Open the [installers]("./installers") folder and install Landis-II and then the necessary extensions in this order:

    1. LANDIS-II-6.1_setup64.exe
    2. LANDIS-II Century Succession 4.0.2-setup.exe
    3. LANDIS-II Base EDA 1.0-setup.exe

2.	Open [BigSur_EDA]("./BigSur_EDA") folder (this is the folder with all of the text and raster files for initial conditions and parameters. They are all filled out with the exact parameters we used for accuracy assessment).

    1. Double click on __SimpleBatchFile.bat__ to start running the model.
    2. Model will take ~3 days running on a computer with multiple cores or more with a single core. Note that the default for the model is use all CPUs so we recommend running this on a virtual machine.
