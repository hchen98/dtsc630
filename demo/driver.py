import pandas as pd
import streamlit as st

CATEGORY = ['Program Management', 'Manufacturing & Supply Chain',
            'Technical Solutions', 'Developer Relations',
            'Hardware Engineering', 'Partnerships',
            'Product & Customer Support', 'Software Engineering',
            'Data Center & Network', 'Business Strategy', 'Technical Writing',
            'Technical Infrastructure', 'IT & Data Management',
            'Marketing & Communications', 'Network Engineering',
            'Sales & Account Management', 'Sales Operations', 'Finance',
            'Legal & Government Relations', 'Administrative',
            'User Experience & Design', 'People Operations',
            'Real Estate & Workplace Services']

uploaded_file = st.file_uploader("Choose a file to upload")

options = st.sidebar.multiselect(
    'What category do you want to select?',
    CATEGORY,
    default=None,
)

if len(options) > 0:
    st.write('You have selected:', options)

if uploaded_file is not None:
    df = pd.read_csv(uploaded_file)
    st.dataframe(df)
