clear all

import delimited "C:/Users/alexa/Dropbox/datasets/summclust1.csv"

summclust treatment, yvar(proposition_vote) xvar(log_income)  cluster(group_id1)

summclust treatment, yvar(proposition_vote) xvar(log_income) fevar(q1_immigration q2_defense) cluster(group_id1)


clear all
import delimited "C:/Users/alexa/Dropbox/datasets/summclust2.csv"

summclust treatment, yvar(proposition_vote) xvar(log_income) fevar(q1_immigration q2_defense) cluster(group_id1) absorb(group_id1)

