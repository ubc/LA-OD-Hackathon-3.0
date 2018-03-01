"""script to aggregate data sets from the Open University Learning Analytics dataset
dataset URL: https://analyse.kmi.open.ac.uk/open_dataset
"""

import numpy as np
import pandas as pd


def summarize_all_enrolments(data_dir, output_filename):
    # create a dataset with one row per registration.

    index_columns = ["code_module", "code_presentation", "id_student"]

    # get registrations
    df_registration = pd.read_csv(data_dir+"studentRegistration.csv",
                                  index_col=index_columns)

    # get demographics
    df_demographics = pd.read_csv(data_dir+"studentInfo.csv",
                                  index_col=index_columns)

    # get the activity log data
    df_activity = pd.read_csv(data_dir+"studentVLE.csv",
                              index_col=index_columns)

    # compute the total activity for students in the course
    df_total_activity = df_activity.groupby(index_columns)["sum_click"].sum()
    df_total_activity.name = "total_activity"

    df_days_active = df_activity.groupby(index_columns)["date"].nunique()
    df_days_active.name = "days_active"

    df_last_active_date = df_activity.groupby(index_columns)["date"].max()
    df_last_active_date.name = "last_active_date"

    df_distinct_content_accessed = df_activity.groupby(index_columns)["id_site"].nunique()
    df_distinct_content_accessed.name = "distinct_content_items_accessed"

    df_assessment_scores = pd.read_csv(data_dir+"studentAssessment.csv")
    df_assessments = pd.read_csv(data_dir+"assessments.csv")
    df_assessments_and_weights = pd.merge(df_assessment_scores, df_assessments)

    df_assessments_and_weights["weighted_score"] = df_assessments_and_weights["weight"]*df_assessments_and_weights["score"]/100

    # treat assessment activities and exams separately
    assessments = df_assessments_and_weights[df_assessments_and_weights["assessment_type"] != "Exam"]
    exams = df_assessments_and_weights[df_assessments_and_weights["assessment_type"] == "Exam"]

    df_final_assessment_score = assessments.groupby(index_columns)["weighted_score"].sum()
    df_final_assessment_score.name = "final_assessment_score"

    df_final_exam_score = exams.groupby(index_columns)["weighted_score"].sum()
    df_final_exam_score.name = "final_exam_score"

    merged = df_registration.join([df_demographics, df_total_activity,
                                   df_days_active, df_last_active_date,
                                   df_distinct_content_accessed,
                                   df_final_assessment_score,
                                   df_final_exam_score])

    # sanity checks
    assert(len(merged) == len(df_registration))
    assert(merged["final_assessment_score"].min() >= 0)
    assert(merged["final_assessment_score"].max() <= 100)
    assert(merged["final_exam_score"].min() >= 0)
    assert(merged["final_exam_score"].max() <= 100)

    merged.to_csv(output_filename)
    return


def summarize_single_course(data_dir, output_filename, code_module, code_presentation):
    # create dataset for a single course offering

    index_columns = ["code_module", "code_presentation", "id_student"]

    # get registrations
    df_registration = pd.read_csv(data_dir + "studentRegistration.csv", index_col=index_columns)

    # get demographics
    df_demographics = pd.read_csv(data_dir + "studentInfo.csv", index_col=index_columns)
    df_registration = df_registration.join(df_demographics)

    # subset to the chosen course offering
    df_registration = df_registration.loc[code_module, code_presentation]

    df_assessments = pd.read_csv(data_dir + "assessments.csv")
    df_assessments = df_assessments[(df_assessments["code_module"] == code_module) & (df_assessments["code_presentation"] == code_presentation)]

    df_assessment_scores = pd.read_csv(data_dir+"studentAssessment.csv")
    df_assessment_scores = df_assessment_scores.merge(df_assessments, on="id_assessment")
    df_assessment_scores["assessment_name"] = (df_assessment_scores["assessment_type"] + "_"
                                               + df_assessment_scores["date"].astype(int).astype(str) + "_"
                                               + df_assessment_scores['id_assessment'].astype(str))

    #reshape grades to one row per student, with columns for each assessment
    wide_grades = df_assessment_scores.pivot(index="id_student",
                                             columns="assessment_name",
                                             values="score")

    # reorder assessment columns by date
    sorted_assessments = sorted(df_assessment_scores["assessment_name"].unique(),
                                key=lambda x: int(x.split('_')[1]))
    wide_grades = wide_grades[sorted_assessments]

    # get activity by day
    df_activity = pd.read_csv(data_dir+"studentVLE.csv")
    df_activity = df_activity[(df_activity["code_module"] == code_module) & (df_activity["code_presentation"] == code_presentation)]

    # reshape aggregate activity data to one row per student, one column per day
    wide_activity = df_activity.pivot_table(index="id_student", columns="date",
                                            values="sum_click", aggfunc=np.sum)
    wide_activity.columns = map(lambda x: "day_"+str(x), wide_activity.columns)

    merged = df_registration.join([wide_grades, wide_activity])

    # sanity checks
    assert(len(merged) == len(df_registration))

    merged.to_csv(output_filename)
    return


if __name__ == "__main__":
    summarize_all_enrolments("anonymisedData/", "all_enrolments.csv")
    summarize_single_course("anonymisedData/", "BBB_2013B_summary.csv",
                            "BBB", "2013B")
