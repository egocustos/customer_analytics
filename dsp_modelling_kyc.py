import numpy as np
from sklearn.model_selection import KFold
from sklearn.utils import resample
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix
from sklearn.linear_model import LogisticRegression
from sklearn.svm import LinearSVC
from xgboost import XGBClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn import metrics
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import roc_curve, auc

kf = KFold(n_splits=10)
kf.get_n_splits(sr_clean)
print(kf)
KFold(n_splits=10, random_state=None, shuffle=False)
i=1
accuracy_nb_score=[]
accuracy_rf_score=[]
accuracy_lr_score=[]
accuracy_svm_score=[]
accuracy_xgb_score=[]
accuracy_dt_score=[]
accuracy_nb_score_neg=[]
accuracy_rf_score_neg=[]
accuracy_lr_score_neg=[]
accuracy_svm_score_neg=[]
accuracy_xgb_score_neg=[]
accuracy_dt_score_neg=[]
for train_index, test_index in kf.split(sr_clean):

    X_train, X_test = sr_clean[train_index], sr_clean[test_index]
    y_train_relevant, y_test_relevant = target_relevant[train_index], target_relevant[test_index]
    y_train_negative, y_test_negative = target_negative[train_index], target_negative[test_index]
    
  

    # concatenate our training data back together
    
    X1 = pd.concat([X_train, y_train_relevant], axis=1)
    X2 = pd.concat([X_train, y_train_negative], axis=1)

    # separate minority and majority classes
    not_relevant = X1[X1.Q2==0]
    relevant = X1[X1.Q2==1]
    not_negative = X2[X2.Q3==0]
    negative = X2[X2.Q3==1]

        # downsample majority
    relevant_downsampled = resample(relevant,
                              replace=True, # sample with replacement
                              n_samples=len(not_relevant), # match number in majority class
                              random_state=42) # reproducible results
    negative_downsampled = resample(negative,
                              replace=True, # sample with replacement
                              n_samples=len(not_negative), # match number in majority class
                              random_state=42) # reproducible results

    # combine majority and upsampled minority
    downsampled_relevant = pd.concat([relevant_downsampled, not_relevant])
    downsampled_negative = pd.concat([negative_downsampled, not_negative])
    
    X_train_relevant=downsampled_relevant['Content']
    X_train_negative=downsampled_negative['Content']
    y_train_relevant=downsampled_relevant['Q2']
    y_train_negative=downsampled_negative['Q3']
    
    vectorizer_tfidf = TfidfVectorizer(stop_words='english', max_df=0.7)
    X_train_relevant = vectorizer_tfidf.fit_transform(downsampled_relevant['Content'].values)
    X_train_negative = vectorizer_tfidf.transform(downsampled_negative['Content'].values)
    X_test = vectorizer_tfidf.transform(X_test.values)

    #Train relevant models
    
    #Naive Bayes relevant
    classifier_nb = MultinomialNB()
    classifier_nb.fit(X_train_relevant, y_train_relevant)
    pred_nb = classifier_nb.predict(X_test)
    #Random Forest relevant
    classifier_rf = RandomForestClassifier(n_estimators = 200, criterion = 'entropy', random_state = 100)
    classifier_rf.fit(X_train_relevant, y_train_relevant)
    pred_rf = classifier_rf.predict(X_test) 
    #Logistic regression relevant
    classifier_lr = LogisticRegression()
    classifier_lr.fit(X_train_relevant, y_train_relevant)
    pred_lr = classifier_lr.predict(X_test) 
    #Super Vector Machine relevant
    classifier_svm = LinearSVC(C=1.0)
    classifier_svm.fit(X_train_relevant, y_train_relevant)
    pred_svm = classifier_svm.predict(X_test) 
    # XGBoost relevant
    xgb_classifier = XGBClassifier(objective="binary:logistic", random_state=30)
    xgb_classifier.fit(X_train_relevant, y_train_relevant)
    pred_xgb = xgb_classifier.predict(X_test) 
    #Decision tree relevant
    classifier_dt = DecisionTreeClassifier(random_state=42)
    classifier_dt.fit(X_train_relevant, y_train_relevant)
    pred_dt= classifier_dt.predict(X_test) 
    
    # Calculate the accuracy score: score
    accuracy_nb = metrics.accuracy_score(y_test_relevant, pred_nb)
    accuracy_rf = metrics.accuracy_score(y_test_relevant, pred_rf)
    accuracy_lr = metrics.accuracy_score(y_test_relevant, pred_lr)
    accuracy_svm = metrics.accuracy_score(y_test_relevant, pred_svm)
    accuracy_xgb = metrics.accuracy_score(y_test_relevant, pred_xgb)
    accuracy_dt = metrics.accuracy_score(y_test_relevant, pred_dt)
    
    
    #Append score per model
    accuracy_nb_score.append(accuracy_nb)
    accuracy_rf_score.append(accuracy_rf)
    accuracy_lr_score.append(accuracy_lr)
    accuracy_svm_score.append(accuracy_svm)
    accuracy_xgb_score.append(accuracy_xgb)
    accuracy_dt_score.append(accuracy_dt)
    
    #Confussion Matrix
    Conf_metrics_nb = metrics.confusion_matrix(y_test_relevant, pred_nb, labels=[0, 1])
    Conf_metrics_rf = metrics.confusion_matrix(y_test_relevant, pred_rf, labels=[0, 1])
    Conf_metrics_lr = metrics.confusion_matrix(y_test_relevant, pred_lr, labels=[0, 1])
    Conf_metrics_svm = metrics.confusion_matrix(y_test_relevant, pred_svm, labels=[0, 1])
    Conf_metrics_xgb = metrics.confusion_matrix(y_test_relevant, pred_xgb, labels=[0, 1])
    Conf_metrics_dt = metrics.confusion_matrix(y_test_relevant, pred_dt, labels=[0, 1])
    
    print("NB accuracy relevant: ","Iteration: ",i," ", accuracy_nb)
    print("RF accuracy relevant: ","Iteration: ",i," ", accuracy_rf)
    print("LR accuracy relevant: ","Iteration: ",i," ", accuracy_lr)
    print("SVM accuracy relevant: ","Iteration: ",i," ", accuracy_svm)
    print("XGB accuracy relevant: ","Iteration: ",i," ", accuracy_xgb)
    print("DT accuracy relevant: ","Iteration: ",i," ",accuracy_dt)
    print("NB Confussion mactrix relevant: ","Iteration: ",i," ",  Conf_metrics_nb)
    print("RF Confussion mactrix relevant: ","Iteration: ",i," ", Conf_metrics_rf)
    print("LR Confussion mactrix relevant: ","Iteration: ",i," ", Conf_metrics_lr)
    print("SVM Confussion mactrix relevant: ","Iteration: ",i," ", Conf_metrics_svm)
    print("XGB Confussion mactrix relevant: ","Iteration: ",i," ", Conf_metrics_xgb)
    print("DT Confussion mactrix relevant: ","Iteration: ",i," ", Conf_metrics_dt)
    print("NB report relevant: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_nb))
    print("RF report relevant: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_rf))
    print("LR report relevant: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_lr))
    print("SVM report relevant: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_svm))
    print("XGB report relevant: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_xgb))
    print("DT report relevant: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_dt))
   
   
    #Train negative models
    
    #Naive Bayes relevant
    classifier_nb_neg = MultinomialNB()
    classifier_nb_neg.fit(X_train_negative, y_train_negative)
    pred_nb_neg = classifier_nb_neg.predict(X_test)
    #Random Forest relevant
    classifier_rf_neg = RandomForestClassifier(n_estimators = 200, criterion = 'entropy', random_state = 100)
    classifier_rf_neg.fit(X_train_negative, y_train_negative)
    pred_rf_neg = classifier_rf_neg.predict(X_test) 
    #Logistic regression relevant
    classifier_lr_neg = LogisticRegression()
    classifier_lr_neg.fit(X_train_negative, y_train_negative)
    pred_lr_neg = classifier_lr_neg.predict(X_test) 
    #Super Vector Machine relevant
    classifier_svm_neg = LinearSVC(C=1.0)
    classifier_svm_neg.fit(X_train_negative, y_train_negative)
    pred_svm_neg = classifier_svm_neg.predict(X_test) 
    # XGBoost relevant
    xgb_classifier_neg = XGBClassifier(objective="binary:logistic", random_state=30)
    xgb_classifier_neg.fit(X_train_negative, y_train_negative)
    pred_xgb_neg = xgb_classifier_neg.predict(X_test) 
    #Decision tree relevant
    classifier_dt_neg = DecisionTreeClassifier(random_state=42)
    classifier_dt_neg.fit(X_train_negative, y_train_negative)
    pred_dt_neg= classifier_dt_neg.predict(X_test) 
    
    # Calculate the accuracy score: score
    accuracy_nb_neg = metrics.accuracy_score(y_test_negative, pred_nb_neg)
    accuracy_rf_neg = metrics.accuracy_score(y_test_negative, pred_rf_neg)
    accuracy_lr_neg = metrics.accuracy_score(y_test_negative, pred_lr_neg)
    accuracy_svm_neg = metrics.accuracy_score(y_test_negative, pred_svm_neg)
    accuracy_xgb_neg = metrics.accuracy_score(y_test_negative, pred_xgb_neg)
    accuracy_dt_neg = metrics.accuracy_score(y_test_negative, pred_dt_neg)
    
    
    #Append score per model
    accuracy_nb_score_neg.append(accuracy_nb_neg)
    accuracy_rf_score_neg.append(accuracy_rf_neg)
    accuracy_lr_score_neg.append(accuracy_lr_neg)
    accuracy_svm_score_neg.append(accuracy_svm_neg)
    accuracy_xgb_score_neg.append(accuracy_xgb_neg)
    accuracy_dt_score_neg.append(accuracy_dt_neg)
    
    #Confussion Matrix
    Conf_metrics_nb_neg = metrics.confusion_matrix(y_test_relevant, pred_nb_neg, labels=[0, 1])
    Conf_metrics_rf_neg = metrics.confusion_matrix(y_test_relevant, pred_rf_neg, labels=[0, 1])
    Conf_metrics_lr_neg = metrics.confusion_matrix(y_test_relevant, pred_lr_neg, labels=[0, 1])
    Conf_metrics_svm_neg = metrics.confusion_matrix(y_test_relevant, pred_svm_neg, labels=[0, 1])
    Conf_metrics_xgb_neg = metrics.confusion_matrix(y_test_relevant, pred_xgb_neg, labels=[0, 1])
    Conf_metrics_dt_neg = metrics.confusion_matrix(y_test_relevant, pred_dt_neg, labels=[0, 1])
    
    print("NB accuracy negative: ","Iteration: ",i," ", accuracy_nb_neg)
    print("RF accuracy negative: ","Iteration: ",i," ", accuracy_rf_neg)
    print("LR accuracy negative: ","Iteration: ",i," ", accuracy_lr_neg)
    print("SVM accuracy negative: ","Iteration: ",i," ", accuracy_svm_neg)
    print("XGB accuracy negative: ","Iteration: ",i," ", accuracy_xgb_neg)
    print("DT accuracy negative: ","Iteration: ",i," ",accuracy_dt_neg)
    print("NB Confussion mactrix negative: ","Iteration: ",i," ",  Conf_metrics_nb_neg)
    print("RF Confussion mactrix negative: ","Iteration: ",i," ", Conf_metrics_rf_neg)
   print("LR Confussion mactrix negative: ","Iteration: ",i," ", Conf_metrics_lr_neg)
    print("SVM Confussion mactrix negative: ","Iteration: ",i," ", Conf_metrics_svm_neg)
    print("XGB Confussion mactrix negative: ","Iteration: ",i," ", Conf_metrics_xgb_neg)
    print("DT Confussion mactrix negative: ","Iteration: ",i," ", Conf_metrics_dt_neg)
    print("NB report negative: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_nb_neg))
    print("RF report negative: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_rf_neg))
    print("LR report negative: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_lr_neg))
    print("SVM report negative: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_svm_neg))
    print("XGB report negative: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_xgb_neg))
    print("DT report negative: ","Iteration: ",i," ", classification_report(y_test_relevant, pred_dt_neg))
    i+=1
    
    n_estimators = [1, 2, 4, 8, 16, 32, 64, 100, 200]
    train_results = []
    test_results = []
    for estimator in n_estimators:
        rf = RandomForestClassifier(n_estimators=estimator, n_jobs=-1)
        rf.fit(X_train_relevant, y_train_relevant)
        train_pred = rf.predict(X_train_relevant)
        false_positive_rate, true_positive_rate, thresholds = roc_curve(y_train_relevant, train_pred)
        roc_auc = auc(false_positive_rate, true_positive_rate)
        train_results.append(roc_auc)
        y_pred = rf.predict(X_test)
        false_positive_rate, true_positive_rate, thresholds = roc_curve(y_test_relevant, y_pred)
        roc_auc = auc(false_positive_rate, true_positive_rate)
        test_results.append(roc_auc)
        
        max_depths = np.linspace(1, 32, 32, endpoint=True)
        train_results2 = []
        test_results2 = []
    for max_depth in max_depths:
        rf2 = RandomForestClassifier(max_depth=max_depth, n_jobs=-1)
        rf2.fit(X_train_relevant, y_train_relevant)
        train_pred2 = rf2.predict(X_train_relevant)
        false_positive_rate2, true_positive_rate2, thresholds2 = roc_curve(y_train_relevant, train_pred2)
        roc_auc2 = auc(false_positive_rate2, true_positive_rate2)
        train_results2.append(roc_auc2)
        y_pred2 = rf2.predict(X_test)
        false_positive_rate2, true_positive_rate2, thresholds2 = roc_curve(y_test_relevant, y_pred2)
        roc_auc2 = auc(false_positive_rate2, true_positive_rate2)
        test_results2.append(roc_auc2)
            
    min_samples_splits = np.linspace(0.1, 1.0, 10, endpoint=True)
    train_results3 = []
    test_results3 = []
    for min_samples_split in min_samples_splits:
        rf3 = RandomForestClassifier(min_samples_split=min_samples_split)
        rf3.fit(X_train_relevant, y_train_relevant)
        train_pred3 = rf3.predict(X_train_relevant)
        false_positive_rate3, true_positive_rate3, thresholds3 = roc_curve(y_train_relevant, train_pred3)
        roc_auc3 = auc(false_positive_rate3, true_positive_rate3)
        train_results3.append(roc_auc3)
        y_pred3 = rf3.predict(X_test)
        false_positive_rate3, true_positive_rate3, thresholds3 = roc_curve(y_test_relevant, y_pred3)
        roc_auc3 = auc(false_positive_rate3, true_positive_rate3)
        test_results3.append(roc_auc3)
        
    min_samples_leafs = np.linspace(0.1, 0.5, 5, endpoint=True)
    train_result4 = []
    test_results4 = []
    for min_samples_leaf in min_samples_leafs:
        rf4 = RandomForestClassifier(min_samples_leaf=min_samples_leaf)
        rf4.fit(X_train_relevant, y_train_relevant)
        train_pred = rf.predict(X_train_relevant)
        false_positive_rate, true_positive_rate, thresholds = roc_curve(y_train, train_pred)
        roc_auc = auc(false_positive_rate, true_positive_rate)
        train_results.append(roc_auc)
        y_pred = rf.predict(X_test)
        false_positive_rate, true_positive_rate, thresholds = roc_curve(y_test, y_pred)
        roc_auc = auc(false_positive_rate, true_positive_rate)
        test_results.append(roc_auc)
        
from matplotlib.legend_handler import HandlerLine2D
line1, = plt.plot(min_samples_leafs, train_results, ‘b’, label=”Train AUC”)
line2, = plt.plot(min_samples_leafs, test_results, ‘r’, label=”Test AUC”)
plt.legend(handler_map={line1: HandlerLine2D(numpoints=2)})
plt.ylabel(‘AUC score’)
plt.xlabel(‘min samples leaf’)
plt.show()
       
from matplotlib.legend_handler import HandlerLine2D
line1, = plt.plot(min_samples_splits, train_results3, 'b', label='Train AUC')
line2, = plt.plot(min_samples_splits, test_results3, 'r', label='Test AUC')
plt.legend(handler_map={line1: HandlerLine2D(numpoints=2)})
plt.ylabel('AUC score')
plt.xlabel('min samples split')
plt.show()
            
            
from matplotlib.legend_handler import HandlerLine2D
line1, = plt.plot(max_depths, train_results2, 'b', label='Train AUC')
line2, = plt.plot(max_depths, test_results2, 'r', label='Test AUC')
plt.legend(handler_map={line1: HandlerLine2D(numpoints=2)})
plt.ylabel('AUC score')
plt.xlabel('Tree depth')
plt.show()
        
from matplotlib.legend_handler import HandlerLine2D
line1, = plt.plot(n_estimators, train_results, 'b', label='Train AUC')
line2, = plt.plot(n_estimators, test_results, 'r', label='Test AUC')
plt.legend(handler_map={line1: HandlerLine2D(numpoints=2)})
plt.ylabel('AUC score')
plt.xlabel('n_estimators')
plt.show()
