
import numpy as np
import pandas as pd
from lightgbm import LGBMClassifier 
from sklearn.model_selection import KFold
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score
import optuna 


all_data= r.all_data



y = all_data.Churn
x_train, x_test, y_train, y_test = train_test_split(all_data.drop("Churn",axis=1), y, test_size=.2)


def objective(trial):
    params = {
        'reg_alpha': trial.suggest_float('reg_alpha', 0.001, 10.0),
        'reg_lambda': trial.suggest_float('reg_lambda', 0.001, 10.0),
        'num_leaves': trial.suggest_int('num_leaves', 11, 333),
        'min_child_samples': trial.suggest_int('min_child_samples', 5, 100),
        "lambda_l1": trial.suggest_loguniform("lambda_l1", 1e-8, 10.0),
        "lambda_l2": trial.suggest_loguniform("lambda_l1", 1e-8, 10.0),
        "feature_fraction": trial.suggest_uniform("feature_fraction", 0.4, 1.0),
        "bagging_fraction": trial.suggest_uniform("bagging_fraction", 0.4, 1.0),
        "bagging_freq": trial.suggest_int("bagging_freq", 1, 7),
        'max_depth': trial.suggest_int('max_depth', 5, 20),
        'learning_rate': trial.suggest_categorical('learning_rate', [0.01, 0.02, 0.05, 0.005, 0.1]),
        'colsample_bytree': trial.suggest_float('colsample_bytree', 0.1, 0.5),
        'n_estimators': trial.suggest_int('n_estimators', 50, 3000),
        'random_state': 42,
        'boosting_type': 'gbdt',
        'metric': 'AUC',
        'device': 'cpu'
    }

    model = LGBMClassifier(**params)
    model.fit(x_train, y_train, eval_set=[(x_test, y_test)], early_stopping_rounds=222, verbose=False)
    y_pred = model.predict_proba(x_test)[:, 1]
    roc_auc = roc_auc_score(y_test, y_pred)

    return roc_auc
from optuna.pruners import SuccessiveHalvingPruner

study = optuna.create_study(direction='maximize',pruner=SuccessiveHalvingPruner())
#study.enqueue_trial(tmp_best_params)
study.optimize(objective, n_trials=100)
print('Number of finished trials:', len(study.trials))
print('Best trial:', study.best_trial.params)
print('Best value:', study.best_value)

paramsLGBM = study.best_trial.params


kf = KFold(n_splits=10, shuffle=True, random_state=42)
x= x_train
y = y_train
test = x_test
auc = []
preds = np.zeros(test.shape[0])
n=0   


for fold, (trn_idx, val_idx) in enumerate(kf.split(x, y)):
    print(f"===== FOLD {fold+1} =====")
    x_train, x_val = x.iloc[trn_idx], x.iloc[val_idx]
    y_train, y_val = y.iloc[trn_idx], y.iloc[val_idx]

    model = LGBMClassifier(**paramsLGBM)

    model.fit(x_train, y_train, eval_set=[(x_val, y_val)], eval_metric='auc', verbose=-1,early_stopping_rounds=500)

    preds += model.predict_proba(test)[:, 1] / kf.n_splits

    auc.append(roc_auc_score(y_val, model.predict_proba(x_val)[:, 1]))
    
np.mean(auc)

roc_auc_score(y_test,preds)

feature_importances_extra = pd.Series(model.feature_importances_,x_train.columns).sort_values(ascending=False)
feature_importances_extra





