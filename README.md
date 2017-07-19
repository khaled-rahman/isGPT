## isGPT: An optimized model to identify sub-Golgi protein types using SVM and random forest based feature selection ##

Necessary Software Tools to install before working with CRISPRpred:
  1. e1071, caTools, ROCR and randomForest R packages
      *You can install it by typing 'install.packages('e1071')' in your R console.
    
#isGPT works in following steps#


#Step 1: It extracts features using featurization.R, trainingset.csv and testingset.csv files.#

    *Open the base.R file and set the file name in it. 

	Your file must be in the same directory as featurization.R file.
    
    *Run base.R file and this will generate features accordingly 

	and save featurized training data in featurized_304_posTrimer.rds and featurized testing data in testFile_64_posTrimer.rds.

#Step 2: Then it runs a wrapper feature selection algorithms of randomForest R package using base.R#

    *The model returned by randomForest will be saved in rfmodel_304_posTrimer.rds file.

#Step 3: In this step, an optimized model is build using createModel.R file.#

    *The best model is saved in svm_304_posTrimer.rds file.

#Step 4: It performs different testing strategies.#

    *Run testModel.R file for independent testing.
    *Run alltestings.R for jackknife testing.
    *Run createModel.R for 10-fold cross-validation which saved resutls in Step 3.
    *All the above files generate results for classification model. All files with '*reg*' or '*regression*' extensions, generate results for regression model.



isGPT tool is developed by Md. Khaledur Rahman.
If you need any help, please contact: khaled.cse.07@gmail.com
