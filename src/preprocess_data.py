import os
import yaml
import numpy as np
import pandas as pd
import argparse

from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer, KNNImputer
from sklearn.preprocessing import StandardScaler, OrdinalEncoder, OneHotEncoder
from sklearn.pipeline import Pipeline, make_pipeline

def read_params(config_path):
    with open(config_path) as yaml_file:
        config = yaml.safe_load(yaml_file)
    return config

def drop_na(df: pd.DataFrame) -> pd.DataFrame:
    return(df.dropna())

def select_fill(df: pd.DataFrame) -> pd.DataFrame:
    # columns_subset = ['behavioral_avoidance', 'behavioral_wash_hands', 'behavioral_large_gatherings',
    #                   'behavioral_outside_home', 'doctor_recc_h1n1', 'doctor_recc_seasonal', 'health_worker',
    #                   'opinion_h1n1_vacc_effective', 'opinion_h1n1_risk', 'opinion_h1n1_sick_from_vacc',
    #                   'opinion_seas_risk', 'opinion_seas_sick_from_vacc', 'education', 'sex', 'income_poverty',
    #                   'marital_status', 'employment_status', 'household_adults', 'employment_industry',
    #                   'employment_occupation']

    num_features = df.columns[df.dtypes != "object"].values
    cat_features = df.columns[df.dtypes == "object"].values

    num_transformer = Pipeline([
        ('impute', SimpleImputer(missing_values=np.nan, strategy = 'median')),
        ('scale', StandardScaler())
    ])

    cat_transformer = Pipeline([
        ('encode', OrdinalEncoder()),
    ])

    preprocessor = ColumnTransformer([
        ('num', num_transformer, num_features),
        ('cat', cat_transformer, cat_features)
    ])

    pipe = make_pipeline(preprocessor)
    df_preprocessed = pipe.fit_transform(df)
    df_preprocessed = pd.DataFrame(df_preprocessed, index = df.index,
                                   columns = df.columns.values)

    return(df_preprocessed)


def onehot_pipeline(df: pd.DataFrame) -> pd.DataFrame:
    num_features = df.columns[df.dtypes != "object"].values
    cat_features = df.columns[df.dtypes == "object"].values

    num_transformer = Pipeline([
        ('scale', StandardScaler()),
        ('impute', KNNImputer(n_neighbors = 10))
    ])

    cat_transformer = Pipeline([
        ('impute', SimpleImputer(strategy = 'constant', fill_value = 'unknown')),
        ('encode', OneHotEncoder(drop = 'first'))
    ])

    preprocessor = ColumnTransformer([
        ('num', num_transformer, num_features),
        ('cat', cat_transformer, cat_features)
    ])

    pipe = make_pipeline(preprocessor)
    df_preprocessed = pipe.fit_transform(df)
    df_preprocessed = pd.DataFrame(df_preprocessed, index = df.index)

    return(df_preprocessed)


def ordinal_pipeline(df: pd.DataFrame) -> pd.DataFrame:
    num_features = df.columns[df.dtypes != "object"].values
    cat_features = df.columns[df.dtypes == "object"].values

    num_transformer = Pipeline([
        ('scale', StandardScaler()),
        ('impute', KNNImputer(n_neighbors = 10))
    ])

    cat_transformer = Pipeline([
        ('impute', SimpleImputer(strategy = 'constant', fill_value = 'unknown')),
        ('encode', OrdinalEncoder())
    ])

    preprocessor = ColumnTransformer([
        ('num', num_transformer, num_features),
        ('cat', cat_transformer, cat_features)
    ])

    pipe = make_pipeline(preprocessor)
    df_preprocessed = pipe.fit_transform(df)
    df_preprocessed = pd.DataFrame(df_preprocessed, index = df.index,
                                   columns = df.columns.values)

    return(df_preprocessed)


def preprocess_data(config_path, is_test_data) -> None:
    config = read_params("params.yaml")
    train_data_path = config["data"]["raw_data"]
    test_data_path = config["data"]["test_competition"]
    train_labels_path = config["data"]["raw_data_labels"]
    random_state = config["base"]["random_state"]
    id_col = config["base"]["id_col"]
    train_preprocessed_path = config["preprocess_data"]["train_path"]
    test_preprocessed_path = config["preprocess_data"]["test_path"]

    train_labels = pd.read_csv(train_labels_path, index_col=id_col)

    if is_test_data == "False":
        data = pd.read_csv(train_data_path, index_col=id_col)
        data_path = train_preprocessed_path
    elif is_test_data == "True":
        data = pd.read_csv(test_data_path, index_col=id_col)
        data_path = test_preprocessed_path

    data_processed = select_fill(data)
    # data_processed = ordinal_pipeline(data)
    
    if is_test_data == "False":
        data_processed = data_processed.join(train_labels)

    data_processed.to_csv(data_path)

if __name__=="__main__":
    args = argparse.ArgumentParser()
    args.add_argument("--config", default="params.yaml")
    args.add_argument("--test", default="False")
    parsed_args = args.parse_args()
    data = preprocess_data(config_path = parsed_args.config,
                           is_test_data = parsed_args.test)
