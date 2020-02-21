import flask
from flask import Response,request
import numpy as np
import pickle
from sklearn.ensemble import RandomForestRegressor

app = flask.Flask(__name__)
app.config["DEBUG"] = True

##########
import numpy as np
import pickle
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
import os

#os.chdir('CUsersSVSS2195Downloads')


@app.route('/creditrisk/', methods=['GET', 'POST'])
def credit_risk():
    test = 0
    filename = 'crm.sav'
    rf_from_pickle = pickle.load(open(filename, 'rb'))

    get_j = request.get_json()
    age = get_j["Age"]
    gender = get_j["gender"]
    Marital = get_j["Marital"]
    NetSalary = get_j["NetSalary"]
    annlincome = get_j["annlincome"]
    YrsInCurrentBusi = get_j["YrsInCurrentBusi"]
    emi = get_j["emi"]
    lnamt = get_j["lnamt"]
    tenore = get_j["tenore"]
    print("input_val=>",age, gender, Marital, NetSalary, annlincome, YrsInCurrentBusi, emi, lnamt, tenore)
    test = np.array([[age, gender, Marital, NetSalary, annlincome, YrsInCurrentBusi, emi, lnamt, tenore]])
    print("test_in=>",test)
    pred = rf_from_pickle.predict(test.reshape(1, -1))
    print(pred)
    if pred <= 0.25:
        return "Very Good Customer"
    elif (pred <= 0.5):
        return "Good Customer"
    elif (pred  < 0.75):
        return "Risk Customer"
    elif (pred >= 0.75):
        return "Very Risk Customer"
# return (str(round(pred[0])))
##########################

app.run(debug=True, port=80)