import numpy as np
import pandas as pd
import matplotlib.pyplot as plt 
import os
import array
import random
from matplotlib import offsetbox
from sklearn.preprocessing import LabelEncoder

questionnaire_map = {
    'Very Slightly or Not At All':1,
    'A Little':2
}

relation_map = {
    'Brother or Sister' : 1
}

teachingexperience_map = {
    'Not at all': 1,
    'A little': 2
}

yesno_map = {
    'Yes': 1,
    'No': 0
}

demographic_map = {
    'High school graduate, diploma or the equivalent (e.g., GED)':0,
    'Some college credit, no degree':1
}

mood_map = {
    'A': 1
}
contribution_map = {
    'I did almost 100%' : 1
}

correct_answer = {
    # zero indexed column indexes
    "pre_knowledge_rock": {
        5: "Igneous"
    },
    "post_knowledge_rock": {
        5: "Igneous"
    }
}

def print_stats(file_path,results_folder, do_categoties = False):
    # Replace this with your own google form name and address!
    path_to_excel_sheet = file_path
    if '.xls' in path_to_excel_sheet:
        df = pd.read_excel(path_to_excel_sheet, header='infer')
    else:
        df = pd.read_csv(path_to_excel_sheet, header='infer')
    # df = df.drop(columns=df.columns[0])

    # Reading the columns that are object
    objCols = df.select_dtypes(['object']).columns
    objCols = objCols[1:] # skip the created_at column

    # Change object columns to category
    try:
        df[objCols] = df[objCols].astype('category')
        # print(objCols)
    except ValueError:
        print("no object to change to category")

    # Mood columns
    mood_columns = ['col1']

    # Code the category columns to have integer values to do statistical analysis
    def code_category_columns(df,objCols):
        for i in objCols:
            coding = df[i].astype("category").cat.codes
            coding = coding.to_frame() 
            coding['original'] = df[i]
            coding = coding.drop_duplicates()
            df[i] = coding
            print(coding)
            print("")

    # Just count the values for each category
    def category_count(df,objCols):
        condition_name = file_path.split("/")[1].split(".")[0].split("_")[1]
        for i in objCols:
            file1 = open("output/textfiles/%s_%s.txt" %(str(df.columns.get_loc(i)),condition_name),"w+") 
            file1.write(str(df[i].value_counts()))
            file1.close() 

    if do_categoties:
        # print count of categories
        category_count(df,objCols)

    # Mean and STD for numerical 
    df.loc['mean'] = df.mean(axis = 0,skipna = True)
    df.loc['std'] = df.std(axis = 0,skipna = True)

    # based on codition
    df_cond = df.loc[df['conditions'] == 1]
    df.loc['mean_adaptive'] = df_cond.mean(axis = 0,skipna = True)
    df.loc['std_adaptive'] = df_cond.std(axis = 0,skipna = True)

    df_cond = df.loc[df['conditions'] == 3]
    df.loc['mean_neutral'] = df_cond.mean(axis = 0,skipna = True)
    df.loc['std_neutral'] = df_cond.std(axis = 0,skipna = True)

    if results_folder != "":
        colcount = 1
        for (colName, columnData) in df.iteritems():
            if (df[colName].dtypes) == "float64":
                df = df.fillna(0)
                df_cond1 = df.loc[df['conditions'] == 1]
                df_cond2 = df.loc[df['conditions'] == 2]
                df_cond3 = df.loc[df['conditions'] == 3]
                
                plt.figure(str(colcount))
                plt.title(colName)
                plotarray = [df_cond1[colName],df_cond3[colName]]
                plt.boxplot(plotarray,showfliers=False)
                positions = (1, 2)
                labels = ("Adaptive", "Neutral")
                plt.xticks(positions, labels)
                plt.savefig(results_folder+str(colcount)+'.png')
                colcount = colcount + 1
                plt.close()

    return df


def clean_audio(file_path):
    path_to_excel_sheet = file_path
    if '.xls' in path_to_excel_sheet:
        df = pd.read_excel(path_to_excel_sheet, header='infer')
    else:
        df = pd.read_csv(path_to_excel_sheet, header='infer')
    df = df.rename(columns={'name': 'user_id'})

    import audio_coding_ids
    mapaudio = audio_coding_ids.mapaudio

    for key, value in mapaudio.items():
        df['user_id'] = df['user_id'].str.replace(key,value,regex=True)
    excluding = ["S3",  "S4",  "S16",  "S19"]
    mask = df['groupname'].isin(excluding)
    df = df[~mask]
    return df

def read_forms(file_path,name):
    path_to_excel_sheet = file_path
    if '.xls' in path_to_excel_sheet:
        df = pd.read_excel(path_to_excel_sheet, header='infer')
    else:
        df = pd.read_csv(path_to_excel_sheet, header='infer')

    # read conditions
    df_cond = pd.read_csv("data/conditions.csv", header='infer')
    conditions = [None]*df.shape[0]
    df['conditions'] = conditions

    # import the coditions
    df = df.rename(columns={"Participant ID (to be filled in by researcher)": 'user_id'})
    for i in range(len(df)) : 
        if df.loc[i,'user_id'] == "Gamma":
            df.loc[i,'user_id'] =  df.loc[i,'user_id'] + df.loc[i,'groupname']
        else:
            wht = df_cond.loc[df_cond['user_id'] == df.loc[i,'user_id'],'condition']
            df.loc[i,'conditions'] = wht.iloc[0]

    df['conditions'].replace({"adaptive":1,"random":2,"neutral":3}, inplace=True)

    # Reading the columns that are object
    objCols = df.select_dtypes(['object']).columns
    objCols = objCols[1:] # skip the created_at column

    # Change object columns to category
    if (len(objCols) > 0):
        df[objCols] = df[objCols].astype('category')

    # Code the category columns to have integer values to do statistical analysis
    def code_category_columns(df,objCols):
        for i in objCols:
            coding = df[i].astype("category").cat.codes
            coding = coding.to_frame() 
            coding['original'] = df[i]
            coding = coding.drop_duplicates()
            df[i] = coding
            # print the coding map
            print(coding)
            print("")

   
    excluding = ["id1",  "id2"]

    mask = df['user_id'].isin(excluding)
    df = df[~mask]

    df.to_csv("output/%s.csv" %str(name),index=False) 

def seperate_multi_select(answer):
    return answer.split(", ")

def forms_stats(form,df_questionnaire):
    
    df_form = pd.read_csv(form)
    print(form)
    df_form['user_id']=df_form.user_id.astype(str)

    user_ids = df_form['user_id'].unique()
    merge_questionnaire = False
    tmp={}
    for col in df_form.columns:
        if '[' in col and ']' in col:
            tmp[col] = col[col.index('[')+1: col.index(']')]

    if len(tmp):
        df_form.rename(columns=tmp, inplace=True)

    if name in correct_answer:
        pre = ['col10']

        post = ['col1']
        
        merge_questionnaire = True
        df_form[f'{name}_correct_points'] = np.nan
        for user_id in user_ids:
            user_row = df_form.loc[df_form.iloc[:,1]==user_id]
            correct_points = 0
            if len(user_row):
                for key, value in correct_answer[name].items():
                    # print(key)
                    if user_row.iloc[0,key] == value:
                        correct_points += 1
                # print(f'points is {correct_points}')
            df_form.loc[df_form['user_id']==user_id, f'{name}_correct_points'] = int(correct_points)
            
    elif "demographic" in name.lower():
        objCols = df_form.select_dtypes(['object']).columns
        pre = ['col1']
        preno =['col2']
        merge_questionnaire = True
        for col in df_form[objCols]:
            if col in preno:
                df_form[col].replace(yesno_map, inplace=True)

        for col in df_form[objCols]:
            if col in pre:
                df_form[col].replace(demographic_map, inplace=True)

    elif "experience" in name.lower():
        objCols = df_form.select_dtypes(['object']).columns
        pre = ['col1']
        partial = ['col2']
        parno = ['cal55']

        for col in df_form[objCols]:
            if col in pre:
                df_form[col].replace(teachingexperience_map, inplace=True)

        for col in df_form[objCols]:
            if col in parno:
                df_form[col].replace(yesno_map, inplace=True)

        for col in df_form[objCols]:
            if col in pre[0]:
                df_form[col].replace(contribution_map, inplace=True)

        for col in df_form[objCols]:
            if col in partial:
                if col == partial[0]:
                    col2 = "fair"
                else:  
                    col2 = "encouraging_teamwork"

                conditions = df_form[col]
                df_form[col2] = conditions

        merge_questionnaire = True

    elif 'perception' in name.lower():
        objCols = df_form.select_dtypes(['object']).columns
        pre = ['col6']
        merge_questionnaire = True

        for col in df_form[objCols]:
            if col in pre:
                df_form[col].replace(teachingexperience_map, inplace=True)
    
    elif 'mood' in name.lower():
        merge_questionnaire = True

    elif 'motivation' in name.lower():
        merge_questionnaire = True
    
    if merge_questionnaire:
        df_form = df_form.drop(columns=['Timestamp'])
        if len(df_questionnaire) != 0:
            if 'conditions' in df_form.columns:
                df_form = df_form.drop(columns=['conditions'])
            df_questionnaire = pd.merge(df_questionnaire, df_form, on='user_id')

        else:
            df_questionnaire = df_form
    return df_questionnaire
    
def read_times(file_path,name):
    ath_to_excel_sheet = file_path
    if '.xls' in path_to_excel_sheet:
        df = pd.read_excel(path_to_excel_sheet, header='infer')
    else:
        df = pd.read_csv(path_to_excel_sheet, header='infer')
    df = df.rename(columns={'name': 'user_id'})

if __name__ == '__main__':

    downloaded_form_names = {
        "demographic": 	"d.csv",
        "pre_experience": 	"2.csv",
        "pre_perception": 	"3.csv",
        "pre_knowledge_rock": 	"4.csv",
        "post_knowledge_rock": 	"5.csv",
        "post_experience": 	"6.csv",
        "post_perception":	"7.csv",
        "post_mood": 	"8.csv",
        "post_motivation": 	"9.csv"
    }

    name_survey_columns = ["wht_id","Unnamed:0","Unnamed:0.1","user_id","gender","age"]
    df = print_stats(f"output/questionnaire.csv","")
    df.to_csv("output/%s.csv" %str("questionnaire2"))
    df_cond_ad = (df.loc[df['conditions'] == 1])
    .drop(df.tail(1).index,inplace=True)
    df_cond_ne = (df.loc[df['conditions'] == 3])
    df_cond_ad = df_cond_ad.drop(columns=df.columns[0])
    df_cond_ad = df_cond_ne.drop(columns=df.columns[0])
    df_cond_ad.to_csv("output/%s.csv" %str("questionnaire_adaptive"))
    df_cond_ne.to_csv("output/%s.csv" %str("questionnaire_neutral"))
    print_stats(f"output/questionnaire_adaptive.csv","",do_categoties=True).to_csv("output/%s.csv" %str("questionnaire_adaptive"))
    print_stats(f"output/questionnaire_neutral.csv","",do_categoties=True).to_csv("output/%s.csv" %str("questionnaire_neutral"))

    read_forms(f"output/audio_analysis.csv",'audio_analysis')
    df = print_stats(f"output/audio_analysis.csv","./output/plots2/")
    df.to_csv("output/%s.csv" %str("audio_analysis2"))

    read_times("output/interaction_time.csv","output/%s.csv"%("times"))

    df = pd.read_csv("output/questionnaire.csv", header='infer')